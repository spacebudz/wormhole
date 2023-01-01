use aiken/builtin
use aiken/bytearray

pub type Either<a, b> {
  Left(a)
  Right(b)
}

pub type Hash =
  ByteArray

pub type Proof =
  List<Either<Hash, Hash>>

fn combine_hash(h1: Hash, h2: Hash) -> Hash {
  builtin.sha2_256(bytearray.concat(h1, h2))
}

pub fn has(e: ByteArray, root: Hash, proof: Proof) -> Bool {
  do_has(builtin.sha2_256(e), root, proof)
}

pub fn do_has(root1: Hash, root: Hash, proof: Proof) -> Bool {
  when proof is {
    [] -> root1 == root
    [Left(l), ..rest] -> do_has(combine_hash(l, root1), root, rest)
    [Right(r), ..rest] -> do_has(combine_hash(root1, r), root, rest)
  }
}
