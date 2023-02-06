use aiken/hash.{Blake2b_224, Hash, Sha2_256}

pub type Either<a, b> {
  Left(a)
  Right(b)
}

pub type Proof =
  List<Either<Sha2_256, Sha2_256>>
