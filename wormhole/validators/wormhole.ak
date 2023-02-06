use aiken/builtin
use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_224, Hash, Sha2_256}
use aiken/list
use aiken/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, OutputReference, ScriptContext, Spend,
  Transaction,
}
use aiken/transaction/credential.{
  Address, PublicKeyCredential, Script, ScriptCredential, VerificationKey,
}
use aiken/transaction/value.{AssetName, PolicyId, Value}
use wormhole/merkletree.{Proof}

type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

type ScriptHash =
  Hash<Blake2b_224, Script>

type Metadata =
  Dict<ByteArray, Data>

type DatumMetadata {
  metadata: Metadata,
  version: Int,
}

type Details {
  extra_output_reference: OutputReference,
  royalty_name: AssetName,
  ip_name: AssetName,
  old_policyid: PolicyId,
  merkle_root: Sha2_256,
  reference_scripthash: ScriptHash,
  lock_scripthash: ScriptHash,
  // A magic number with 0 utility
  nonce: Int,
}

type Action {
  Mint(List<Proof>)
  Burn
  MintExtra
}

const labe_length = 4

fn checked_mint(proofs: List<Proof>, tx: Transaction, details: Details) -> Bool {
  True
}

fn checked_burn(tx: Transaction) -> Bool {
  True
}

fn checked_extra(tx: Transaction) -> Bool {
  True
}

fn mint(details: Details, action: Action, ctx: ScriptContext) -> Bool {
  let tx = ctx.transaction
  when action is {
    Mint(proofs) -> checked_mint(proofs, tx, details)
    Burn -> checked_burn(tx)
    MintExtra -> checked_extra(tx)
  }
}
