<p align="center">
  <img width="100%" src="./src/images/wormhole.png" align="center"/>
</p>

# Wormhole

The SpaceBudz wormhole contract. Moving from CIP-0025 to CIP-0068.

## Official SpaceBudz Policy

Policy Id: **`N/A`**\
⚠️ The contract is not finalized yet

## Requirements

- Deno >= 1.28.1

## Test

1. **Mock old SpaceBudz:**

Old SpaceBudz follow CIP-0025 and have the following asset name structure: `SpaceBud{id}` (e.g. `SpaceBud123`). But we leave out the metadata since they are not relevant for testing the contract.

```ts
import { Lucid, Blockfrost, MintingPolicy } from "https://deno.land/x/lucid@0.7.8/mod.ts";

const lucid = await Lucid.new(new Blockfrost(...), "Preview");

lucid.selectWalletFromSeed(
  "<seed_phrase>",
);

const { paymentCredential } = lucid.utils.getAddressDetails(
  await lucid.wallet.address(),
);

const mockOldPolicy: MintingPolicy = {
  type: "Native",
  script: toHex(
    C.NativeScript.new_script_pubkey(
      C.ScriptPubkey.new(C.Ed25519KeyHash.from_hex(paymentCredential?.hash!)),
    ).to_bytes(),
  ),
};

const mockOldPolicyId = lucid.utils.validatorToScriptHash(mockOldPolicy);

export async function mockMint() {
  const tx = await lucid.newTx()
    .mintAssets({
      [mockOldPolicyId + utf8ToHex(`SpaceBud${0}`)]: 1n,
      [mockOldPolicyId + utf8ToHex(`SpaceBud${1}`)]: 1n,
      [mockOldPolicyId + utf8ToHex(`SpaceBud${2}`)]: 1n,
      [mockOldPolicyId + utf8ToHex(`SpaceBud${3}`)]: 1n,
    })
    .attachMintingPolicy(mockOldPolicy)
    .complete();

  const signedTx = await tx.sign().complete();
  return signedTx.submit();
};

console.log(await mockMint());
```

2. **Init contract and deploy scripts**

```ts
import { Lucid, Blockfrost } from "https://deno.land/x/lucid@0.7.8/mod.ts";
import { Contract } from "./src/offchain.ts";

const lucid = await Lucid.new(new Blockfrost(...), "Preview");

lucid.selectWalletFromSeed(
  "<seed_phrase>",
);

const contract = new Contract(lucid, {
  extraOutRef: {
    txHash: "<tx_hash>",
    outputIndex: 0,
  },
  oldPolicyId: "<mock_old_policy_id>",
})

// Deploy scripts on-chain, which can be reused in all contract interactions to reduce fees.
console.log(await contract.deployScripts())
```

`extraOutRef` can be ignored for now and initialized with empty arguments. This parameter is needed to mint a unique Royalty (Label 500) and Intellectual Property (Label 600) token under the same policy id.

3. **Re-init contract and migrate**

Add the tx hash from the `deployScripts` endpoint to the `Contract` config.

```ts
const contract = new Contract(lucid, {
  extraOutRef: {
    txHash: "<tx_hash>",
    outputIndex: 0,
  },
  oldPolicyId: "<mock_old_policy_id>",
  deployTxHash: "<tx_hash_from_deployScripts>"
})

// Migrate SpaceBud1 
console.log(await contract.migrate([1]))
```
⚠️ You can migrate multiple SpaceBudz at a time, but 3-4 is the limit. More than that exceeds the execution unit costs. Two ways to improve that:
- Transaction chaining
- Rewrite the contract in a different contract language like Aiken. PlutusTx is not very efficient.
- Make the conract more efficient in general?

4. **Burn (just for fun and testing)**
```ts
console.log(await contract.burn(1));
```

## Compile contract
```
deno task build:contract
```
See [requirements](./src/ghc/README.md).

## Bundle for NPM
```
deno task build
```
Outputs a `dist` folder.
Lucid needs to be imported separately and is a peer dependency (version `@0.7.8`).

## Contract endpoints

```ts
migrate(ids: number[]): Promise<TxHash>
```
```ts
deployScripts(): Promise<TxHash>
```
```ts
burn(id: number): Promise<TxHash> 
```
```ts
getMetadata(id: number): Promise<Json>
```
```ts
hasMigrated(id: number): Promise<boolean>
```
```ts
getDeployedScripts(): Promise<{ mint: UTxO }> 
```

## TODO

- [ ] Move away from PlutusTx -> Aiken.
- [ ] Listen to chain to show migration event on Twitter (Ogmios).