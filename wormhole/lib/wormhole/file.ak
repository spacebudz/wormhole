use aiken/dict
use aiken/interval.{Interval, IntervalBound, PositiveInfinity}
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, PublicKeyCredential, ScriptCredential, StakeCredential,
}
use aiken/transaction/value

const keyhash = #[1, 2, 3, 4, 5, 6]

const scripthash = #[6, 5, 4, 3, 2, 1]

pub fn keyhash_address(with_stake_credential: Option<StakeCredential>) {
  Address {
    payment_credential: PublicKeyCredential(keyhash),
    stake_credential: with_stake_credential,
  }
}

pub fn scripthash_address(with_stake_credential: Option<StakeCredential>) {
  Address {
    payment_credential: ScriptCredential(scripthash),
    stake_credential: with_stake_credential,
  }
}

type SampleData {
  a: Int,
  b: ByteArray,
}

pub fn tx_1() -> Transaction {
  let sample_datum = SampleData { a: 1, b: #[1] }
  // let sample_data: Data = sample_datum
  let tx =
    Transaction {
      inputs: [
        Input {
          output_reference: OutputReference {
            transaction_id: TransactionId { hash: #[] },
            output_index: 0,
          },
          output: Output {
            address: scripthash_address(None),
            value: value.zero(),
            datum: InlineDatum(sample_datum),
            reference_script: None,
          },
        },
      ],
      reference_inputs: [],
      outputs: [
        Output {
          address: keyhash_address(None),
          value: value.from_lovelace(10000),
          datum: InlineDatum(sample_datum),
          reference_script: None,
        },
      ],
      fee: value.zero(),
      mint: value.from_asset(#[0, 0, 0], #[0], -1),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: Interval {
        lower_bound: IntervalBound {
          bound_type: PositiveInfinity,
          is_inclusive: True,
        },
        upper_bound: IntervalBound {
          bound_type: PositiveInfinity,
          is_inclusive: True,
        },
      },
      extra_signatories: [keyhash],
      redeemers: dict.new(),
      datums: dict.new(),
      id: TransactionId { hash: #[] },
    }
  tx
}

test some_test() {
  tx_1() == tx_1()
}
