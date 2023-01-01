import {
  Assets,
  Emulator,
  fromText,
  generateSeedPhrase,
  Lucid,
} from "https://deno.land/x/lucid@0.8.4/mod.ts";
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
  [oldPolicyId + fromText(`SpaceBud${9999}`)]: 1n,
});
const ACCOUNT_1 = await generateAccount({ lovelace: 75000000000n });

const emulator = new Emulator([ACCOUNT_0, ACCOUNT_1]);

const lucid = await Lucid.new(emulator);

lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase);

// ---- SETUP

const deployTxHash = await new Contract(lucid, {
  extraOutRef: { txHash: "", outputIndex: 0 },
  oldPolicyId,
}).deployScripts();

await lucid.awaitTx(deployTxHash);

const contract = new Contract(lucid, {
  extraOutRef: { txHash: "", outputIndex: 0 },
  oldPolicyId,
  deployTxHash,
});

// ---- SETUP

Deno.test("Migrate", async () => {
  await lucid.selectWalletFromSeed(ACCOUNT_0.seedPhrase).awaitTx(
    await contract.migrate([0, 1, 13, 9999]),
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
