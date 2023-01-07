import {
  Assets,
  Emulator,
  fromText,
  generateSeedPhrase,
  Lucid,
} from "https://deno.land/x/lucid@0.8.7/mod.ts";
import { Contract } from "../mod.ts";
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.145.0/testing/asserts.ts";

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
  [oldPolicyId + fromText(`SpaceBud${0}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${1}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${13}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${444}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${600}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${702}`)]: 1n,
  [oldPolicyId + fromText(`SpaceBud${9999}`)]: 1n,
});
const ACCOUNT_1 = await generateAccount({ lovelace: 75000000000n });

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

// ---- SETUP

Deno.test("Mint extra (Royalty and Ip)", async () => {
  await lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase).awaitTx(
    await contract.mintExtra(),
  );
});

Deno.test("Update royalty", async () => {
  lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase);
  const tx = await contract.updateRoyalty([{
    address: ACCOUNT_0.address,
    fee: 0.016,
    fixedFee: 2000000n,
  }]);
  const sig = await lucid.selectWalletFromSeed(ACCOUNT_1.seedPhrase).fromTx(tx)
    .partialSign();
  const multisig0 = await lucid.selectWalletFromSeed(MULTISIG_0.seedPhrase)
    .fromTx(tx).partialSign();
  const multisig1 = await lucid.selectWalletFromSeed(MULTISIG_1.seedPhrase)
    .fromTx(tx).partialSign();
  const signedTx = await lucid.fromTx(tx).assemble([sig, multisig0, multisig1])
    .complete();
  await lucid.awaitTx(await signedTx.submit());
});

Deno.test("Migrate", async () => {
  await lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase).awaitTx(
    await contract.migrate([0, 1, 13, 444, 600, 702, 9999]),
  );
  assert(await contract.hasMigrated(0));
  assert(await contract.hasMigrated(1));
  assertEquals(await contract.hasMigrated(299), false);
});

Deno.test("Burn", async () => {
  await lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase).awaitTx(
    await contract.burn(0),
  );
  assertEquals(await contract.hasMigrated(0), false);
});

Deno.test("Move", async () => {
  await lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase).awaitTx(
    await contract.move(1),
  );
});
