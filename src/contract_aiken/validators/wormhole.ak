use aiken/transaction.{OutputReference, ScriptContext}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{PolicyId}
use merkletree.{Hash, Proof}

pub type Action {
  Mint(List<Proof>)
  Burn
  MintExtra
}

pub type Details {
  extra_oref: OutputReference,
  royalty_name: ByteArray,
  ip_name: ByteArray,
  old_policyid: PolicyId,
  merkle_root: Hash,
  reference_address: Address,
  lock_address: Address,
}

pub fn mint(
  contract_details: Details,
  action: Action,
  ctx: ScriptContext,
) -> Bool {
  when action is {
    Mint(proofs) ->
      when proofs is {
        [proof, ..rest] ->
          merkletree.has(#[1, 2, 3], contract_details.merkle_root, proof)
        _ -> False
      }
    Burn -> False
    MintExtra -> False
  }
}
