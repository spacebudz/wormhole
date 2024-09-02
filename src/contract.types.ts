import { Data } from "../deps.ts";

export const MetadataSchema = Data.Map(Data.Bytes(), Data.Any());
export type Metadata = Data.Static<typeof MetadataSchema>;
export const Metadata = MetadataSchema as unknown as Metadata;

export const DatumMetadataSchema = Data.Object({
  metadata: MetadataSchema,
  version: Data.Integer({ minimum: 1, maximum: 1 }),
  extra: Data.Any(),
});
export type DatumMetadata = Data.Static<typeof DatumMetadataSchema>;
export const DatumMetadata = DatumMetadataSchema as unknown as DatumMetadata;

export const OutRefSchema = Data.Object({
  txHash: Data.Object({ hash: Data.Bytes({ minLength: 32, maxLength: 32 }) }),
  outputIndex: Data.Integer(),
});
export type OutRef = Data.Static<typeof OutRefSchema>;
export const OutRef = OutRefSchema as unknown as OutRef;

export const Hash = Data.Object({
  hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export type Hash = Data.Static<typeof Hash>;

export const DetailsParamsSchema = Data.Tuple([
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 100
  Data.Object({
    extraOref: OutRefSchema,
    royaltyName: Data.Bytes(),
    ipName: Data.Bytes(),
    oldPolicyId: Data.Bytes({ minLength: 28, maxLength: 28 }),
    merkleRoot: Hash,
    refAddress: Data.Bytes({ minLength: 28, maxLength: 28 }),
    lockAddress: Data.Bytes({ minLength: 28, maxLength: 28 }),
    nonce: Data.Integer(),
  }),
]);
export type DetailsParams = Data.Static<typeof DetailsParamsSchema>;
export const DetailsParams = DetailsParamsSchema as unknown as DetailsParams;

export const RefParamsSchema = Data.Tuple([
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 1
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 100
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 222
]);
export type RefParams = Data.Static<typeof RefParamsSchema>;
export const RefParams = RefParamsSchema as unknown as RefParams;

export const LockParamsSchema = Data.Tuple([
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 1
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 100
  Data.Bytes({ minLength: 4, maxLength: 4 }), // label 222
  Data.Bytes({ minLength: 28, maxLength: 28 }),
]);
export type LockParams = Data.Static<typeof LockParamsSchema>;
export const LockParams = LockParamsSchema as unknown as LockParams;

export const ProofSchema = Data.Array(Data.Enum([
  Data.Object({ Left: Data.Tuple([Hash]) }),
  Data.Object({ Right: Data.Tuple([Hash]) }),
]));
export type Proof = Data.Static<typeof ProofSchema>;
export const Proof = ProofSchema as unknown as Proof;

export const ActionSchema = Data.Enum([
  Data.Object({ Mint: Data.Tuple([Data.Array(ProofSchema)]) }),
  Data.Literal("Burn"),
  Data.Literal("MintExtra"),
]);
export type Action = Data.Static<typeof ActionSchema>;
export const Action = ActionSchema as unknown as Action;

export const RefActionSchema = Data.Enum([
  Data.Literal("Burn"),
  Data.Literal("Move"),
]);
export type RefAction = Data.Static<typeof RefActionSchema>;
export const RefAction = RefActionSchema as unknown as RefAction;

export const CredentialSchema = Data.Enum([
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
export type Credential = Data.Static<typeof CredentialSchema>;
export const Credential = CredentialSchema as unknown as Credential;

export const AddressSchema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(Data.Enum([
    Data.Object({ Inline: Data.Tuple([CredentialSchema]) }),
    Data.Object({
      Pointer: Data.Tuple([Data.Object({
        slotNumber: Data.Integer(),
        transactionIndex: Data.Integer(),
        certificateIndex: Data.Integer(),
      })]),
    }),
  ])),
});
export type Address = Data.Static<typeof AddressSchema>;
export const Address = AddressSchema as unknown as Address;

export const RoyaltyRecipientSchema = Data.Object({
  address: AddressSchema,
  fee: Data.Integer({ minimum: 1 }),
  minFee: Data.Nullable(Data.Integer()),
  maxFee: Data.Nullable(Data.Integer()),
});
export type RoyaltyRecipient = Data.Static<typeof RoyaltyRecipientSchema>;
export const RoyaltyRecipient =
  RoyaltyRecipientSchema as unknown as RoyaltyRecipient;

export const RoyaltyInfoSchema = Data.Object({
  recipients: Data.Array(RoyaltyRecipientSchema),
  version: Data.Integer({ minimum: 1, maximum: 1 }),
  extra: Data.Any(),
});
export type RoyaltyInfo = Data.Static<typeof RoyaltyInfoSchema>;
export const RoyaltyInfo = RoyaltyInfoSchema as unknown as RoyaltyInfo;
