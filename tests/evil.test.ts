import {
  Assets,
  Data,
  Emulator,
  fromText,
  generateSeedPhrase,
  Lucid,
  PolicyId,
  toHex,
  toUnit,
} from "https://deno.land/x/lucid@0.9.2/mod.ts";
import { Contract } from "../mod.ts";
import { assert } from "https://deno.land/std@0.145.0/testing/asserts.ts";
import * as D from "../src/contract.types.ts";
import metadata from "../src/data/metadata.json" assert { type: "json" };

async function generateAccount(assets: Assets) {
  const seedPhrase = generateSeedPhrase();
  return {
    seedPhrase,
    address: await (await Lucid.new(undefined, "Custom"))
      .selectWalletFromSeed(seedPhrase).wallet.address(),
    assets,
  };
}

const oldPolicyId = "11".repeat(28);

const ACCOUNT_0 = await generateAccount({
  lovelace: 30000000000n,
  [oldPolicyId + fromText(`SpaceBud${13}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${444}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${600}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${702}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${9999}`)]: 1n,
});
const ACCOUNT_1 = await generateAccount({
  [oldPolicyId + fromText(`SpaceBud${0}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${1}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${123}`)]: 2n, // assuming this is a twin
  lovelace: 75000000000n,
});

const MULTISIG_0 = await generateAccount({ lovelace: 100000000n });
const MULTISIG_1 = await generateAccount({ lovelace: 100000000n });
const MULTISIG_2 = await generateAccount({ lovelace: 100000000n });
const MULTISIG_3 = await generateAccount({ lovelace: 100000000n });

const emulator = new Emulator([
  ACCOUNT_0,
  ACCOUNT_1,
  MULTISIG_0,
  MULTISIG_1,
  MULTISIG_2,
  MULTISIG_3,
]);

const lucid = await Lucid.new(emulator);

lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase);

// ---- SETUP

const [extraUtxo] = await lucid.utxosAt(ACCOUNT_1.address);

const deployTxHash = await new Contract(lucid, {
  extra: {
    outRef: { txHash: extraUtxo.txHash, outputIndex: extraUtxo.outputIndex },
    initialOwners: [
      MULTISIG_0.address,
      MULTISIG_1.address,
      MULTISIG_2.address,
      MULTISIG_3.address,
    ],
  },
  oldPolicyId,
}).deployScripts();

await lucid.awaitTx(deployTxHash);

const contract = new Contract(lucid, {
  extra: {
    outRef: { txHash: extraUtxo.txHash, outputIndex: extraUtxo.outputIndex },
    initialOwners: [
      MULTISIG_0.address,
      MULTISIG_1.address,
      MULTISIG_2.address,
      MULTISIG_3.address,
    ],
  },
  oldPolicyId,
  deployTxHash,
});

Deno.test("Good migration", async () => {
  lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase);
  await lucid.awaitTx(await contract.migrate([0, 1, 123]));
  lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase);
  await lucid.awaitTx(await contract.migrate([123]));
  lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase);
  await lucid.awaitTx(await contract.migrate([13]));
});

// ---- SETUP

Deno.test("Evil migration failed", async () => {
  const ids = [9999, 444, 600, 702];
  const refScripts = await contract.getDeployedScripts();

  // Order is important since the contract relies on this.
  const orderedIds = ids.slice().sort((a, b) => b - a);

  const datas = orderedIds.map((id) => contract.data[id]);
  const proofs = datas.map((d) => contract.merkleTree.getProof(d));

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
      [toUnit(contract.mintPolicyId, fromText(`Bud${id}`), 100)]: 1n,
      [toUnit(contract.mintPolicyId, fromText(`Bud${id}`), 222)]: 2n,
    },
  }), {});

  const lockAssets: Assets = orderedIds.reduce((prev, id) => ({
    ...prev,
    ...{
      [toUnit(contract.config.oldPolicyId, fromText(`SpaceBud${id}`))]: 1n,
    },
  }), {});

  let failed = false;
  try {
    const tx = await lucid.newTx()
      .mintAssets(mintAssets, action)
      .compose((() => {
        const tx = lucid.newTx();
        orderedIds.forEach((id) => {
          tx.payToContract(
            contract.referenceAddress,
            Data.to<D.DatumMetadata>({
              metadata: Data.castFrom<D.Metadata>(
                Data.fromJson(metadata[id]),
                D.Metadata,
              ),
              version: 1n,
              extra: Data.from(Data.void()),
            }, D.DatumMetadata),
            {
              [toUnit(contract.mintPolicyId, fromText(`Bud${id}`), 100)]: 1n,
            },
          );
        });
        return tx;
      })())
      .payToContract(contract.lockAddress, {
        inline: Data.to<PolicyId>(contract.mintPolicyId, Data.String),
      }, lockAssets)
      .compose(
        refScripts.mint
          ? lucid.newTx().readFrom([refScripts.mint])
          : lucid.newTx().attachSpendingValidator(contract.mintPolicy),
      )
      .complete();
  } catch (_) {
    failed = true;
  }
  assert(failed);
});

Deno.test("Evil burn failed", async () => {
  const id = 13;
  const [refNFTUtxo] = await lucid.utxosAtWithUnit(
    contract.referenceAddress,
    toUnit(contract.mintPolicyId, fromText(`Bud${id}`), 100),
  );

  if (!refNFTUtxo) throw new Error("NoUTxOError");

  const refScripts = await contract.getDeployedScripts();

  let failed = false;
  try {
    const tx = await lucid.newTx()
      .collectFrom([refNFTUtxo], Data.to<D.RefAction>("Burn", D.RefAction))
      .mintAssets({
        [toUnit(contract.mintPolicyId, fromText(`Bud${id}`), 222)]: -1n,
      }, Data.to<D.Action>("Burn", D.Action))
      .attachSpendingValidator(contract.referenceValidator)
      .compose(
        refScripts.mint
          ? lucid.newTx().readFrom([refScripts.mint])
          : lucid.newTx().attachSpendingValidator(contract.mintPolicy),
      ).complete();
  } catch (_) {
    failed = true;
  }
  assert(failed);
});

Deno.test("Evil move failed", async () => {
  lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase);
  let failed = false;
  try {
    const id = 1;
    const [refNFTUtxo] = await lucid.utxosAtWithUnit(
      contract.referenceAddress,
      toUnit(contract.mintPolicyId, fromText(`Bud${13}`), 100),
    );

    const ownershipUtxo = await lucid.wallet.getUtxos().then((utxos) =>
      utxos.find((utxo) =>
        utxo.assets[toUnit(contract.mintPolicyId, fromText(`Bud${id}`), 222)]
      )
    );

    if (!ownershipUtxo) throw new Error("NoOwnershipError");
    if (!refNFTUtxo) throw new Error("NoUTxOError");

    const datum = await lucid.datumOf(refNFTUtxo);

    const tx = await lucid.newTx()
      .collectFrom([refNFTUtxo], Data.to<D.RefAction>("Move", D.RefAction))
      .collectFrom([ownershipUtxo])
      .payToContract(
        refNFTUtxo.address,
        datum,
        { ...refNFTUtxo.assets, lovelace: 0n },
      )
      .attachSpendingValidator(contract.referenceValidator)
      .complete();

    const signedTx = await tx.sign().complete();

    await lucid.awaitTx(await signedTx.submit());
  } catch (_) {
    failed = true;
  }
  assert(failed);
});

Deno.test("Evil unlocking failed", async () => {
  lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase);
  let failed = false;
  try {
    const id = 123;
    const oldBud = toUnit(
      contract.config.oldPolicyId,
      fromText(`SpaceBud${id}`),
    );

    const [lockUtxo] = await lucid.utxosAtWithUnit(
      contract.lockAddress,
      oldBud,
    );

    const remainingAssets: Assets = Object.fromEntries(
      Object.entries(lockUtxo.assets).filter(([unit, _]) =>
        unit !== oldBud && unit !== "lovelace"
      ),
    );

    const datum = await lucid.datumOf(lockUtxo);

    const tx = await lucid.newTx()
      .collectFrom([lockUtxo], Data.void())
      .compose(
        Object.keys(remainingAssets).length > 0
          ? (() => {
            return lucid.newTx()
              .payToContract(
                lockUtxo.address,
                { inline: datum },
                Object.fromEntries(Object.entries(remainingAssets).slice(0, 1)),
              );
          })()
          : null,
      )
      .compose(await contract._burn(id))
      .attachSpendingValidator(contract.lockValidator)
      .complete();
  } catch (_) {
    failed = true;
  }
  assert(failed);
});
