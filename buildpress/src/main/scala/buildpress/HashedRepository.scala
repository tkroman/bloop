package buildpress

final case class HashedRepository(get: Repository, hash: Option[Int])
