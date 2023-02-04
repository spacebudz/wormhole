import {
  Address,
  Lovelace,
  OutRef,
  PolicyId,
  TxHash,
} from "https://deno.land/x/lucid@0.8.9/mod.ts";

export type ContractConfig = {
  extra: { outRef: OutRef; initialOwners: Address[] };
  oldPolicyId: PolicyId;
  deployTxHash?: TxHash;
};

export type RoyaltyRecipient = {
  address: Address;
  /** e.g.: 0.04 (4%) */
  fee: number;
  /** Optionally set a minimum absolute fee. */
  minFee?: Lovelace;
  /** Optionally set a maximum absolute fee. */
  maxFee?: Lovelace;
};
