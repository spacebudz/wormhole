import { Data } from "../deps.ts";

export const Metadata = Data.Map(Data.Bytes(), Data.Any());
export type Metadata = Data.Static<typeof Metadata>;

export const DatumMetadata = Data.Object({
  metadata: Metadata,
  version: Data.Integer({ minimum: 1, maximum: 1 }),
  extra: Data.Any(),
});
export type DatumMetadata = Data.Static<typeof DatumMetadata>;

export const OutRef = Data.Object({
  txHash: Data.Object({ hash: Data.Bytes({ minLength: 32, maxLength: 32 }) }),
  outputIndex: Data.Integer(),
});
export type OutRef = Data.Static<typeof OutRef>;

export const Hash = Data.Object({
  hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export type Hash = Data.Static<typeof Hash>;

export const DetailsParams = Data.Tuple([
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 100
  Data.Object({
    extraOref: OutRef,
    royaltyName: Data.Bytes(),
    ipName: Data.Bytes(),
    oldPolicyId: Data.Bytes({ minLength: 28, maxLength: 28 }),
    merkleRoot: Hash,
    refAddress: Data.Bytes({ minLength: 28, maxLength: 28 }),
    lockAddress: Data.Bytes({ minLength: 28, maxLength: 28 }),
    nonce: Data.Integer(),
  }),
]);
export type DetailsParams = Data.Static<typeof DetailsParams>;

export const RefParams = Data.Tuple([
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 1
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 100
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 222
]);
export type RefParams = Data.Static<typeof RefParams>;

export const LockParams = Data.Tuple([
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 1
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 100
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 222
  Data.Bytes({ minLength: 28, maxLength: 28 }),
]);
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
  Data.Object({
    PublicKeyCredential: Data.Tuple([
      Data.Bytes({ minLength: 28, maxLength: 28 }),
    ]),
  }),
  Data.Object({
    ScriptCredential: Data.Tuple([
      Data.Bytes({ minLength: 28, maxLength: 28 }),
    ]),
  }),
]);
export type Credential = Data.Static<typeof Credential>;

export const Address = Data.Object({
  paymentCredential: Credential,
  stakeCredential: Data.Nullable(Data.Enum([
    Data.Object({ Inline: Data.Tuple([Credential]) }),
    Data.Object({
      Pointer: Data.Tuple([Data.Object({
        slotNumber: Data.Integer(),
        transactionIndex: Data.Integer(),
        certificateIndex: Data.Integer(),
      })]),
    }),
  ])),
});
export type Address = Data.Static<typeof Address>;

export const RoyaltyRecipient = Data.Object({
  address: Address,
  fee: Data.Integer({ minimum: 1 }),
  minFee: Data.Nullable(Data.Integer()),
  maxFee: Data.Nullable(Data.Integer()),
});
type RoyaltyRecipient = Data.Static<typeof RoyaltyRecipient>;

export const RoyaltyInfo = Data.Object({
  recipients: Data.Array(RoyaltyRecipient),
  version: Data.Integer({ minimum: 1, maximum: 1 }),
  extra: Data.Any(),
});
export type RoyaltyInfo = Data.Static<typeof RoyaltyInfo>;
