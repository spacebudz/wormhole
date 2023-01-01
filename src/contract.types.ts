import { Data } from "https://deno.land/x/lucid@0.8.4/mod.ts";

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
