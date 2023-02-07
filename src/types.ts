import {
  Address,
  Lovelace,
  OutRef,
  PolicyId,
  TxHash,
} from "https://deno.land/x/lucid@0.9.1/mod.ts";

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
  minFee?: Lovelace | null;
  /** Optionally set a maximum absolute fee. */
  maxFee?: Lovelace | null;
};
