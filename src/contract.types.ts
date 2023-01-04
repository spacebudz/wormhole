import { Data } from "https://deno.land/x/lucid@0.8.5/mod.ts";

export const DatumMetadata = Data.Object({
  metadata: Data.Any,
  version: Data.BigInt,
});
export type DatumMetadata = Data.Static<typeof DatumMetadata>;

export const OutRef = Data.Object({
  txHash: Data.Object({ hash: Data.String }),
  outputIndex: Data.BigInt,
});
export type OutRef = Data.Static<typeof OutRef>;

export const Hash = Data.Object({ hash: Data.String });
export type Hash = Data.Static<typeof Hash>;

export const DetailsParams = Data.Tuple([Data.Object({
  extraOref: OutRef,
  royaltyName: Data.String,
  ipName: Data.String,
  oldPolicyId: Data.String,
  merkleRoot: Hash,
  refAddress: Data.String,
  lockAddress: Data.String,
})]);
export type DetailsParams = Data.Static<typeof DetailsParams>;

export const RefParams = Data.Tuple([Data.String]);
export type RefParams = Data.Static<typeof RefParams>;

export const LockParams = Data.Tuple([Data.String]);
export type LockParams = Data.Static<typeof LockParams>;

export const Proof = Data.Array(Data.Enum([
  Data.Object({ Left: Data.Tuple([Hash]) }),
  Data.Object({ Right: Data.Tuple([Hash]) }),
]));
export type Proof = Data.Static<typeof Proof>;

export const Action = Data.Enum([
  Data.Object({ Mint: Data.Tuple([Data.Array(Proof)]) }),
  Data.Literal("Burn"),
  Data.Literal("MintExtra"),
]);
export type Action = Data.Static<typeof Action>;

export const RefAction = Data.Enum([
  Data.Literal("Burn"),
  Data.Literal("Move"),
]);
export type RefAction = Data.Static<typeof RefAction>;

export const Credential = Data.Enum([
  Data.Object({ PublicKeyCredential: Data.Tuple([Data.String]) }),
  Data.Object({ ScriptCredential: Data.Tuple([Data.String]) }),
]);
export type Credential = Data.Static<typeof Credential>;

export const Address = Data.Object({
  paymentCredential: Credential,
  stakeCredential: Data.Nullable(Data.Enum([
    Data.Object({ Inline: Data.Tuple([Credential]) }),
    Data.Object({
      Pointer: Data.Tuple([Data.Object({
        slotNumber: Data.BigInt,
        transactionIndex: Data.BigInt,
        certificateIndex: Data.BigInt,
      })]),
    }),
  ])),
});
export type Address = Data.Static<typeof Address>;

export const RoyaltyRecipient = Data.Object({
  address: Address,
  fee: Data.BigInt,
  fixedFee: Data.BigInt,
});
type RoyaltyRecipient = Data.Static<typeof RoyaltyRecipient>;

export const RoyaltyInfo = Data.Object({
  recipients: Data.Array(RoyaltyRecipient),
  minAda: Data.BigInt,
});
export type RoyaltyInfo = Data.Static<typeof RoyaltyInfo>;
