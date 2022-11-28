import {
  Address,
  applyParamsToScript,
  Assets,
  concat,
  Constr,
  Data,
  fromHex,
  Json,
  Lucid,
  MerkleTree,
  MintingPolicy,
  PlutusData,
  PolicyId,
  SpendingValidator,
  toLabel,
  toUnit,
  TxHash,
  utf8ToHex,
  UTxO,
} from "https://deno.land/x/lucid@0.7.8/mod.ts";
import scripts from "./ghc/scripts.json" assert { type: "json" };
import metadata from "./data/metadata.json" assert { type: "json" };
import { Action, addressToData, ContractConfig } from "./utils.ts";

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
      script: scripts.reference,
    };

    this.referenceAddress = this.lucid.utils.validatorToAddress(
      this.referenceValidator,
    );

    this.lockValidator = {
      type: "PlutusV2",
      script: scripts.lock,
    };

    this.lockAddress = this.lucid.utils.validatorToAddress(this.lockValidator);

    this.data = metadata.map((m) =>
      concat(
        fromHex(toLabel(222) + utf8ToHex(`Bud${m.id}`)),
        fromHex(toLabel(100) + utf8ToHex(`Bud${m.id}`)),
        fromHex(toLabel(100) + utf8ToHex(`Bud${m.id}`)),
        new TextEncoder().encode(`SpaceBud${m.id}`),
        fromHex(
          lucid.utils.datumToHash(
            Data.to(new Constr(0, [Data.fromJson(m), 1n])),
          ),
        ), // metadata
      )
    );

    this.merkleTree = new MerkleTree(this.data);

    this.mintPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript(
        scripts.mint,
        new Constr(0, [toLabel(100), toLabel(222)]),
        new Constr(0, [
          new Constr(0, [
            new Constr(0, [this.config.extraOutRef.txHash]),
            BigInt(this.config.extraOutRef.outputIndex),
          ]),
          toLabel(500) + utf8ToHex("Royalty"),
          toLabel(600) + utf8ToHex("Ip"),
          this.config.oldPolicyId,
          new Constr(0, [this.merkleTree.rootHash()]), //??
          addressToData(this.referenceAddress),
          addressToData(this.lockAddress),
        ]),
      ),
    };

    this.mintPolicyId = this.lucid.utils.mintingPolicyToId(this.mintPolicy);
  }

  async migrate(ids: number[]): Promise<TxHash> {
    const refScripts = await this.getDeployedScripts();

    // Order is important since the contract relies on this.
    const orderedIds = ids.toSorted();

    const datas = orderedIds.map((id) => this.data[id]);
    const proofs = datas.map((d) => this.merkleTree.getProof(d));

    const mintRedeemer = Data.to(
      new Constr(0, [
        proofs.map((proof) =>
          proof.map((p) =>
            p.left
              ? new Constr(0, [new Constr(0, [p.left])])
              : new Constr(1, [new Constr(0, [p.right!])])
          )
        ),
      ]),
    );

    const mintAssets: Assets = orderedIds.reduce((prev, id) => ({
      ...prev,
      ...{
        [toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 100)]: 1n,
        [toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 222)]: 1n,
      },
    }), {});

    const tx = await this.lucid.newTx().mintAssets(mintAssets, mintRedeemer)
      .apply((thisTx) => {
        orderedIds.reverse().forEach((id) => {
          thisTx.payToContract(
            this.referenceAddress,
            Data.to(new Constr(0, [Data.fromJson(metadata[id]), 1n])),
            {
              [toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 100)]: 1n,
            },
          );
          thisTx.payToContract(this.lockAddress, {
            inline: Data.to(this.mintPolicyId),
          }, {
            [toUnit(this.config.oldPolicyId, utf8ToHex(`SpaceBud${id}`))]: 1n,
          });
        });
      })
      .readFrom([refScripts.mint]).complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async burn(id: number): Promise<TxHash> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 100),
    );

    if (!refNFTUtxo) throw new Error("NoUTxOError");

    const refScripts = await this.getDeployedScripts();

    const tx = await this.lucid.newTx()
      .collectFrom([refNFTUtxo], Data.empty())
      .mintAssets({
        [toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 100)]: -1n,
        [toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 222)]: -1n,
      }, Action.BurnNFT)
      .attachSpendingValidator(this.referenceValidator)
      .readFrom([refScripts.mint])
      .complete();

    const signedTx = await tx.sign().complete();
    return signedTx.submit();
  }

  async hasMigrated(id: number): Promise<boolean> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 100),
    );

    if (!refNFTUtxo) return false;
    return true;
  }

  async getMetadata(id: number): Promise<Json> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, utf8ToHex(`Bud${id}`), 100),
    );

    if (!refNFTUtxo) return {};

    const metadataDatum = Data.from(
      await this.lucid.datumOf(refNFTUtxo),
    ) as Constr<
      PlutusData
    >;

    const metadata: {
      name: string;
      image: string;
      id: number;
    } = Data
      .toJson(metadataDatum.fields[0]);
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
