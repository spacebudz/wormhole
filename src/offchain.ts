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
  sha256,
  SpendingValidator,
  toHex,
  toLabel,
  toUnit,
  Transaction,
  Tx,
  TxHash,
  UTxO,
} from "../deps.ts";
import scripts from "./ghc/scripts.json" assert { type: "json" };
import metadata from "./data/metadata.json" assert { type: "json" };
import merkleTreeData from "./data/merkletree_data.json" assert {
  type: "json",
};
import { budConfig } from "./config.ts";
import { ContractConfig, RoyaltyRecipient } from "./types.ts";
import * as D from "./contract.types.ts";
import { fromAddress, toAddress } from "./utils.ts";

// The twin SpaceBudz add unfortunately some extra complexity to the contract. But whatever.. it's solvable.
const TWIN0 = 1903;
const TWIN1 = 6413;

function isTwin(id: number) {
  return id === TWIN0 || id === TWIN1;
}

export class Contract {
  lucid: Lucid;
  referenceValidator: SpendingValidator;
  referenceAddress: Address;
  lockValidator: SpendingValidator;
  lockAddress: Address;
  extraMultisig: SpendingValidator;
  extraAddress: Address;
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
    config: ContractConfig = budConfig,
  ) {
    this.lucid = lucid;
    this.config = config;

    this.referenceValidator = {
      type: "PlutusV2",
      script: applyParamsToScript<D.RefParams>(scripts.reference, [
        toLabel(1),
        toLabel(100),
        toLabel(222),
      ], D.RefParams),
    };

    this.referenceAddress = this.lucid.utils.validatorToAddress(
      this.referenceValidator,
    );

    this.lockValidator = {
      type: "PlutusV2",
      script: applyParamsToScript<D.LockParams>(scripts.lock, [
        toLabel(1),
        toLabel(100),
        toLabel(222),
        this.config.oldPolicyId,
      ], D.LockParams),
    };

    this.lockAddress = this.lucid.utils.validatorToAddress(this.lockValidator);

    this.extraMultisig = this.lucid.utils.nativeScriptFromJson({
      type: "atLeast",
      required: Math.ceil(this.config.extra.initialOwners.length / 2),
      scripts: this.config.extra.initialOwners.map((owner) => {
        const { paymentCredential } = this.lucid.utils.getAddressDetails(owner);
        if (!paymentCredential?.hash || paymentCredential.type === "Script") {
          throw new Error(
            "Owner needs to be a public key address, or address is invalid.",
          );
        }
        return {
          type: "sig",
          keyHash: paymentCredential.hash,
        };
      }),
    });

    this.extraAddress = this.lucid.utils.validatorToAddress(this.extraMultisig);

    // We have to make an exception for the twins.
    // 'Reference' NFTs for them are minted with a different label.
    // They are just some mock NFTs (with label 1 instead of 100) in order to keep the contract as it is.
    // The actual reference NFTs for the twins are manually preminted.

    // For perfomance reason we opt out and use a json file.
    // this.data = metadata.map((m, id) =>
    //   concat(
    //     fromHex(toLabel(222) + fromText(`Bud${id}`)),
    //     fromHex(toLabel(isTwin(id) ? 1 : 100) + fromText(`Bud${id}`)),
    //     fromHex(toLabel(isTwin(id) ? 1 : 100) + fromText(`Bud${id}`)),
    //     new TextEncoder().encode(`SpaceBud${id}`),
    //     fromHex(
    //       lucid.utils.datumToHash(
    //         Data.to<D.DatumMetadata>({
    //           metadata: Data.castFrom<D.Metadata>(Data.fromJson(m), D.Metadata),
    //           version: 1n,
    //           extra: Data.from(Data.void()),
    //         }, D.DatumMetadata),
    //       ),
    //     ), // metadata
    //   )
    // );

    // Careful! This one works with the official SpaceBudz policy
    this.data = merkleTreeData.map((d) => fromHex(d));

    this.merkleTree = new MerkleTree(this.data);

    this.mintPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript<D.DetailsParams>(
        scripts.mint,
        [
          toLabel(100),
          {
            extraOref: {
              txHash: { hash: this.config.extra.outRef.txHash },
              outputIndex: BigInt(this.config.extra.outRef.outputIndex),
            },
            royaltyName: toLabel(500) + fromText("Royalty"),
            ipName: toLabel(600) + fromText("Ip"),
            oldPolicyId: this.config.oldPolicyId,
            merkleRoot: { hash: toHex(this.merkleTree.rootHash()) },
            refAddress: this.lucid.utils.validatorToScriptHash(
              this.referenceValidator,
            ),
            lockAddress: this.lucid.utils.validatorToScriptHash(
              this.lockValidator,
            ),
            nonce: 23321n,
          },
        ],
        D.DetailsParams,
      ),
    };

    this.mintPolicyId = this.lucid.utils.mintingPolicyToId(this.mintPolicy);
  }

  /** Mint Royalty and IP token. We also mint the reference NFT UTxOs for the twins manually, this is an expection, but needs to be done! */
  async mintExtra(): Promise<TxHash> {
    const refScripts = await this.getDeployedScripts();

    const [extraUtxo] = await this.lucid.utxosByOutRef([
      this.config.extra.outRef,
    ]);

    if (!extraUtxo) throw new Error("NoUTxOError");

    const royaltyToken = toUnit(this.mintPolicyId, fromText(`Royalty`), 500);
    const ipToken = toUnit(this.mintPolicyId, fromText(`Ip`), 600);

    const twin0Token = toUnit(this.mintPolicyId, fromText(`Bud${TWIN0}`), 100);
    const twin1Token = toUnit(this.mintPolicyId, fromText(`Bud${TWIN1}`), 100);

    const tx = await this.lucid.newTx()
      .collectFrom([extraUtxo])
      .mintAssets({
        [royaltyToken]: 1n,
        [ipToken]: 1n,
        [twin0Token]: 1n,
        [twin1Token]: 1n,
      }, Data.to<D.Action>("MintExtra", D.Action))
      .payToAddress(this.extraAddress, { [royaltyToken]: 1n })
      .payToAddress(this.extraAddress, { [ipToken]: 1n })
      .payToContract(
        this.referenceAddress,
        Data.to<D.DatumMetadata>({
          metadata: Data.castFrom<D.Metadata>(
            Data.fromJson(metadata[TWIN0]),
            D.Metadata,
          ),
          version: 1n,
          extra: Data.from(Data.void()),
        }, D.DatumMetadata),
        {
          [twin0Token]: 1n,
        },
      )
      .payToContract(
        this.referenceAddress,
        Data.to<D.DatumMetadata>({
          metadata: Data.castFrom<D.Metadata>(
            Data.fromJson(metadata[TWIN1]),
            D.Metadata,
          ),
          version: 1n,
          extra: Data.from(Data.void()),
        }, D.DatumMetadata),
        {
          [twin1Token]: 1n,
        },
      )
      .compose(
        refScripts.mint
          ? this.lucid.newTx().readFrom([refScripts.mint])
          : this.lucid.newTx().attachSpendingValidator(this.mintPolicy),
      )
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  /**
   * Update the Intellectual Property of SpaceBudz.
   * Specifiy a URL that points to a document describing the IP.
   * The data behind the URL will be fetched and hashed.
   * The URL and hash will be part of the datum.
   */
  async updateIp(url: string): Promise<Transaction> {
    const hash = toHex(
      await fetch(url)
        .then((res) => res.arrayBuffer())
        .then(
          (arrayBuffer) => sha256(new Uint8Array(arrayBuffer)),
        ),
    );

    const Ip = Data.Object({ url: Data.Bytes(), hash: Data.Bytes() });
    type Ip = Data.Static<typeof Ip>;

    const ipDatum = Data.to<Ip>({ url: fromText(url), hash }, Ip);

    const [ipUtxo] = await this.lucid.utxosAtWithUnit(
      this.extraAddress,
      toUnit(this.mintPolicyId, fromText("Ip"), 600),
    );

    if (!ipUtxo) throw new Error("NoUTxOError");

    return (await this.lucid.newTx()
      .collectFrom([ipUtxo])
      .payToAddressWithData(ipUtxo.address, ipDatum, ipUtxo.assets)
      .attachSpendingValidator(this.extraMultisig)
      .complete())
      .toString();
  }

  async updateRoyalty(
    royaltyRecipients: RoyaltyRecipient[],
  ): Promise<Transaction> {
    const [royaltyUtxo] = await this.lucid.utxosAtWithUnit(
      this.extraAddress,
      toUnit(this.mintPolicyId, fromText("Royalty"), 500),
    );

    if (!royaltyUtxo) throw new Error("NoUTxOError");

    const royaltyDatum = Data.to<D.RoyaltyInfo>({
      recipients: royaltyRecipients.map((recipient) => ({
        address: fromAddress(recipient.address),
        fee: BigInt(Math.floor(1 / (recipient.fee / 10))),
        minFee: recipient.minFee || null,
        maxFee: recipient.maxFee || null,
      })),
      version: 1n,
      extra: Data.from(Data.void()),
    }, D.RoyaltyInfo);

    return (await this.lucid.newTx()
      .collectFrom([royaltyUtxo])
      .payToAddressWithData(
        royaltyUtxo.address,
        royaltyDatum,
        royaltyUtxo.assets,
      )
      .attachSpendingValidator(this.extraMultisig)
      .complete())
      .toString();
  }

  /** Return the datum of the UTxO the royalty token is locked in. */
  async getRoyaltyInfo(): Promise<{ utxo: UTxO; royaltyInfo: D.RoyaltyInfo }> {
    const utxo = await this.lucid.utxoByUnit(
      toUnit(this.mintPolicyId, fromText("Royalty"), 500),
    );
    if (!utxo) throw new Error("Royalty info not found.");

    return {
      utxo,
      royaltyInfo: await this.lucid.datumOf<D.RoyaltyInfo>(utxo, D.RoyaltyInfo),
    };
  }

  async getRoyalty(): Promise<RoyaltyRecipient[]> {
    const { royaltyInfo } = await this.getRoyaltyInfo();

    return royaltyInfo.recipients.map((recipient) => ({
      address: toAddress(recipient.address, this.lucid),
      fee: 10 / Number(recipient.fee),
      minFee: recipient.minFee,
      maxFee: recipient.maxFee,
    }));
  }

  async migrate(ids: number[]): Promise<TxHash> {
    const refScripts = await this.getDeployedScripts();

    // Order is important since the contract relies on this.
    // We are sorting ids (as string not as number) in reverse/descending order
    const orderedIds = ids.slice().sort().reverse();

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
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), isTwin(id) ? 1 : 100)]:
          1n,
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
              metadata: Data.castFrom<D.Metadata>(
                Data.fromJson(metadata[id]),
                D.Metadata,
              ),
              version: 1n,
              extra: Data.from(Data.void()),
            }, D.DatumMetadata),
            {
              [
                toUnit(
                  this.mintPolicyId,
                  fromText(`Bud${id}`),
                  isTwin(id) ? 1 : 100,
                )
              ]: 1n,
            },
          );
        });
        return tx;
      })())
      .payToContract(this.lockAddress, {
        inline: Data.to<PolicyId>(this.mintPolicyId, Data.Bytes()),
      }, lockAssets)
      .compose(
        refScripts.mint
          ? this.lucid.newTx().readFrom([refScripts.mint])
          : this.lucid.newTx().attachSpendingValidator(this.mintPolicy),
      )
      .complete();

    const txSigned = await tx.sign().complete();
    return txSigned.submit();
  }

  async _burn(id: number): Promise<Tx> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, fromText(`Bud${id}`), isTwin(id) ? 1 : 100),
    );

    if (!refNFTUtxo) throw new Error("NoUTxOError");

    const refScripts = await this.getDeployedScripts();

    return this.lucid.newTx()
      .collectFrom([refNFTUtxo], Data.to<D.RefAction>("Burn", D.RefAction))
      .mintAssets({
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), isTwin(id) ? 1 : 100)]:
          -1n,
        [toUnit(this.mintPolicyId, fromText(`Bud${id}`), 222)]: -1n,
      }, Data.to<D.Action>("Burn", D.Action))
      .attachSpendingValidator(this.referenceValidator)
      .compose(
        refScripts.mint
          ? this.lucid.newTx().readFrom([refScripts.mint])
          : this.lucid.newTx().attachSpendingValidator(this.mintPolicy),
      );
  }

  async burn(id: number): Promise<TxHash> {
    const tx = await (await this._burn(id)).complete();

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

    const tx = await this.lucid.newTx()
      .collectFrom([refNFTUtxo], Data.to<D.RefAction>("Move", D.RefAction))
      .collectFrom([ownershipUtxo])
      .payToContract(
        refNFTUtxo.address,
        datum,
        { ...refNFTUtxo.assets, lovelace: 0n },
      )
      .attachSpendingValidator(this.referenceValidator)
      .complete();

    const signedTx = await tx.sign().complete();

    return signedTx.submit();
  }

  async hasMigrated(id: number): Promise<boolean> {
    const [refNFTUtxo] = await this.lucid.utxosAtWithUnit(
      this.referenceAddress,
      toUnit(this.mintPolicyId, fromText(`Bud${id}`), isTwin(id) ? 1 : 100),
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

  async getDeployedScripts(): Promise<{ mint: UTxO | null }> {
    if (!this.config.deployTxHash) return { mint: null };
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
