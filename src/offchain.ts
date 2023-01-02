import {
  Address,
  applyParamsToScript,
  Assets,
  concat,
  Data,
  fromHex,
  fromText,
  Json,
  Lucid,
  MerkleTree,
  MintingPolicy,
  PolicyId,
  SpendingValidator,
  toHex,
  toLabel,
  toUnit,
  TxHash,
  UTxO,
} from "https://deno.land/x/lucid@0.8.4/mod.ts";
import scripts from "./ghc/scripts.json" assert { type: "json" };
import metadata from "./data/metadata.json" assert { type: "json" };
import { ContractConfig } from "./utils.ts";
import * as D from "./contract.types.ts";

export class Contract {
  lucid: Lucid;
  referenceValidator: SpendingValidator;
  referenceAddress: Address;
  lockValidator: SpendingValidator;
  lockAddress: Address;
  mintPolicy: MintingPolicy;
  mintPolicyId: PolicyId;
  config: ContractConfig;
  merkleTree: MerkleTree;
  data: Uint8Array[];

  /**
   * **NOTE**: config.oldPolicyId and config.extraOutRef are parameters of the migration contract.
   * Changing this parameter changes the plutus scripts and so the script hashes!
   */
  constructor(
    lucid: Lucid,
    config: ContractConfig,
  ) {
    this.lucid = lucid;
    this.config = config;

    this.referenceValidator = {
      type: "PlutusV2",
      script: applyParamsToScript<D.RefParams>(scripts.reference, [
        toLabel(222),
      ], D.RefParams),
    };

    this.referenceAddress = this.lucid.utils.validatorToAddress(
      this.referenceValidator,
    );

    this.lockValidator = {
      type: "PlutusV2",
      script: applyParamsToScript<D.LockParams>(scripts.lock, [
        this.config.oldPolicyId,
      ], D.LockParams),
    };

    this.lockAddress = this.lucid.utils.validatorToAddress(this.lockValidator);

    this.data = metadata.map((m) =>
      concat(
        fromHex(toLabel(222) + fromText(`Bud${m.id}`)),
        fromHex(toLabel(100) + fromText(`Bud${m.id}`)),
        fromHex(toLabel(100) + fromText(`Bud${m.id}`)),
        new TextEncoder().encode(`SpaceBud${m.id}`),
        fromHex(
          lucid.utils.datumToHash(
            Data.to<D.DatumMetadata>({
              metadata: Data.fromJson(m),
              version: 1n,
            }, D.DatumMetadata),
          ),
        ), // metadata
      )
    );

    this.merkleTree = new MerkleTree(this.data);

    this.mintPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript<D.DetailsParams>(
        scripts.mint,
        [
          {
            extraOref: {
              txHash: { hash: this.config.extraOutRef.txHash },
              outputIndex: BigInt(this.config.extraOutRef.outputIndex),
            },
            royaltyName: fromText("Royalty"),
            ipName: fromText("Ip"),
            oldPolicyId: this.config.oldPolicyId,
            merkleRoot: { hash: toHex(this.merkleTree.rootHash()) },
            refAddress:
              this.lucid.utils.getAddressDetails(this.referenceAddress)
                .paymentCredential!.hash,
            lockAddress: this.lucid.utils.getAddressDetails(this.lockAddress)
              .paymentCredential!.hash,
          },
        ],
        D.DetailsParams,
      ),
    };

    this.mintPolicyId = this.lucid.utils.mintingPolicyToId(this.mintPolicy);
  }

  async migrate(ids: number[]): Promise<TxHash> {
    const refScripts = await this.getDeployedScripts();

    // Order is important since the contract relies on this.
    const orderedIds = ids.toSorted().reverse();

    const datas = orderedIds.map((id) => this.data[id]);
    const proofs = datas.map((d) => this.merkleTree.getProof(d));

    const action = Data.to<D.Action>({
      Mint: [
        proofs.map((proof) =>
          proof.map((p) =>
            p.left
              ? { Left: [{ hash: toHex(p.left) }] }
              : { Right: [{ hash: toHex(p.right!) }] }
          )
        ),
      ],
    }, D.Action);

    const mintAssets: Assets = orderedIds.reduce((prev, id) => ({
      ...prev,
      ...{
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100)]: 1n,
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), 222)]: 1n,
      },
    }), {});

    const lockAssets: Assets = orderedIds.reduce((prev, id) => ({
      ...prev,
      ...{
        [toUnit(this.config.oldPolicyId, fromText(`SpaceBud${id}`))]: 1n,
      },
    }), {});

    const tx = await this.lucid.newTx()
      .mintAssets(mintAssets, action)
      .compose((() => {
        const tx = this.lucid.newTx();
        orderedIds.forEach((id) => {
          tx.payToContract(
            this.referenceAddress,
            Data.to<D.DatumMetadata>({
              metadata: Data.fromJson(metadata[id]),
              version: 1n,
            }, D.DatumMetadata),
            {
              [toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100)]: 1n,
            },
          );
        });
        return tx;
      })())
      .payToContract(this.lockAddress, {
        inline: Data.to<PolicyId>(this.mintPolicyId, Data.String),
      }, lockAssets)
      .readFrom([refScripts.mint]).complete();

    const txSigned = await tx.sign().complete();

    return txSigned.submit();
  }

  async burn(id: number): Promise<TxHash> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100),
    );

    if (!refNFTUtxo) throw new Error("NoUTxOError");

    const refScripts = await this.getDeployedScripts();

    const tx = await this.lucid.newTx()
      .collectFrom([refNFTUtxo], Data.void())
      .mintAssets({
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100)]: -1n,
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), 222)]: -1n,
      }, Data.to<D.Action>("Burn", D.Action))
      .attachSpendingValidator(this.referenceValidator)
      .readFrom([refScripts.mint])
      .complete();

    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  /** This endpoint doesn't do more than moving the ref NFT to eventually extract min ADA (e.g. protocol parameters changed). */
  async move(id: number): Promise<TxHash> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100),
    );

    const ownershipUtxo = await this.lucid.wallet.getUtxos().then((utxos) =>
      utxos.find((utxo) =>
        utxo.assets[toUnit(this.mintPolicyId, fromText(`Bud${id}`), 222)]
      )
    );

    if (!ownershipUtxo) throw new Error("NoOwnershipError");
    if (!refNFTUtxo) throw new Error("NoUTxOError");

    const datum = await this.lucid.datumOf(refNFTUtxo);

    delete refNFTUtxo.assets.lovelace;

    const tx = await this.lucid.newTx()
      .collectFrom([refNFTUtxo], Data.to<D.RefAction>("Move", D.RefAction))
      .collectFrom([ownershipUtxo])
      .payToContract(
        refNFTUtxo.address,
        datum,
        refNFTUtxo.assets,
      )
      .attachSpendingValidator(this.referenceValidator)
      .complete();

    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  async hasMigrated(id: number): Promise<boolean> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100),
    );

    if (!refNFTUtxo) return false;
    return true;
  }

  async getMetadata(id: number): Promise<Json> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, fromText(`Bud${id}`), 100),
    );

    if (!refNFTUtxo) return {};

    const datumMetadata = Data.from<D.DatumMetadata>(
      await this.lucid.datumOf(refNFTUtxo),
      D.DatumMetadata,
    );

    const metadata: {
      name: string;
      image: string;
      id: number;
    } = Data
      .toJson(datumMetadata.metadata);
    return metadata;
  }

  async getDeployedScripts(): Promise<{ mint: UTxO }> {
    if (!this.config.deployTxHash) throw new Error("Scripts are not deployed.");
    const [mint] = await this.lucid.utxosByOutRef([{
      txHash: this.config.deployTxHash,
      outputIndex: 0,
    }]);
    return { mint };
  }

  /** Deploy necessary scripts to reduce tx costs. */
  async deployScripts(): Promise<TxHash> {
    const tx = await this.lucid.newTx()
      .payToAddressWithData(this.lockAddress, {
        scriptRef: this.mintPolicy,
      }, {}).complete();

    const txSigned = await tx.sign().complete();

    console.log("\n⛓ Deploy Tx Hash:", txSigned.toHash());
    console.log(
      "You can now paste the Tx Hash into the Contract config.\n",
    );

    return txSigned.submit();
  }
}
