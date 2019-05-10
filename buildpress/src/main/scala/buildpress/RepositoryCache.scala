package buildpress

import buildpress.io.AbsolutePath
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.io.IOException
import scala.collection.mutable

final case class RepositoryCache(indexLocation: AbsolutePath, entries: List[HashedRepository]) {
  def getCachedRepoFor(target: Repository): Option[HashedRepository] =
    entries.find(_.get.id == target.id)

  def merge(newRepositories: List[HashedRepository]): RepositoryCache = {
    val mergedRepositories = new mutable.ListBuffer[HashedRepository]
    mergedRepositories.++=(newRepositories)
    for (entry <- entries if newRepositories.exists(_.get.id == entry.get.id)) {
      mergedRepositories.+=(entry)
    }
    RepositoryCache(indexLocation, mergedRepositories.toList)
  }
}

object RepositoryCache {
  def empty(source: AbsolutePath): RepositoryCache = RepositoryCache(source, Nil)

  def persist(cache: RepositoryCache): Either[BuildpressError.PersistFailure, Unit] = {
    val cacheFileContents = new StringBuilder()
    cache.entries.foreach { entry =>
      cacheFileContents
        .++=(entry.get.id)
        .++=(",")
        .++=(entry.get.uri.toASCIIString)
        .++=(System.lineSeparator())
    }
    try {
      Files.write(
        cache.indexLocation.underlying,
        cacheFileContents.mkString.getBytes(StandardCharsets.UTF_8)
      )
      Right(())
    } catch {
      case t: IOException =>
        val msg =
          s"Unexpected error when persisting cache file ${cache.indexLocation}: '${t.getMessage}'"
        Left(BuildpressError.PersistFailure(error(msg), Some(t)))
    }
  }
}
