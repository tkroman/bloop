package buildpress

import java.io.{IOException, InputStream, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal
import bloop.launcher.core.Shell
import buildpress.io.{AbsolutePath, BuildpressPaths}
import buildpress.util.Traverse.either._
import caseapp.core.{Messages, WithHelp}

abstract class Buildpress(
    in: InputStream,
    out: PrintStream,
    err: PrintStream,
    shell: Shell,
    explicitBuildpressHome: Option[AbsolutePath],
    implicit val cwd: AbsolutePath
) {
  type EitherErrorOr[T] = Either[BuildpressError, T]
  def exit(exitCode: Int): Unit

  import BuildpressParams.buildpressParamsParser
  implicit val messagesParams =
    Messages.messages[BuildpressParams].copy(appName = "buildpress", progName = "buildpress")
  implicit val messagesParamsHelp = messagesParams.withHelp

  def run(args: Array[String]): Unit = {
    def errorAndExit(msg: String): Unit = { err.println(msg); exit(1) }

    BuildpressParams.buildpressParamsParser.withHelp.detailedParse(args) match {
      case Left(a) => errorAndExit(error(a))
      case Right((WithHelp(usage, help, result), remainingArgs, extraArgs)) =>
        if (help) out.println(messagesParams.helpMessage)
        if (usage) out.println(messagesParams.usageMessage)
        result match {
          case Left(parserError) => errorAndExit(error(parserError))
          case Right(params) =>
            if (!params.buildpressFile.exists) {
              errorAndExit(s"Input file '${params.buildpressFile}' doesn't exist")
            } else {
              press(params) match {
                case Right(_) => ()
                // Verbose, but we will enrich failure handling in the future, so required
                case Left(f: BuildpressError.CloningFailure) => errorAndExit(f.msg)
                case Left(f: BuildpressError.ImportFailure) => errorAndExit(f.msg)
                case Left(f: BuildpressError.InvalidBuildpressHome) => errorAndExit(f.msg)
                case Left(f: BuildpressError.ParseFailure) => errorAndExit(f.msg)
                case Left(f: BuildpressError.PersistFailure) => errorAndExit(f.msg)
              }
            }
        }
    }
  }

  def press(params: BuildpressParams): EitherErrorOr[Unit] = {
    for {
      home <- validateBuildpressHome(params.buildpressHome)
      cache <- parseRepositoryCache(home)
      clonedRepositories <- parseAndCloneRepositories(params, home, cache)
      newCacheAfterCloning = cache.merge(clonedRepositories.map(_._1))
      _ <- RepositoryCache.persist(newCacheAfterCloning)
      bloopConfigDirs <- exportRepositories(params, clonedRepositories.map(_._2))
    } yield {
      out.println(s"Cache file ${newCacheAfterCloning.indexLocation}")
      bloopConfigDirs.foreach { configDir =>
        out.println(success(s"Generated $configDir"))
      }
      out.println(s"😎  Buildpress finished successfully")
    }
  }

  def validateBuildpressHome(
      homeDir: AbsolutePath
  ): Either[BuildpressError.InvalidBuildpressHome, AbsolutePath] = {
    if (homeDir.getParent.exists) {
      if (!homeDir.exists) Files.createDirectory(homeDir.underlying)
      Right(homeDir)
    } else {
      // We don't create the parent of the buildpress home out of precaution
      val msg =
        s"Detected buildpress home '${homeDir.syntax}' cannot be created, its parent doesn't exist"
      Left(BuildpressError.InvalidBuildpressHome(error(msg)))
    }
  }

  def parseRepositoryCache(home: AbsolutePath): EitherErrorOr[RepositoryCache] = {
    val buildpressCacheFile = home.resolve("buildpress.out")
    if (!buildpressCacheFile.exists) Right(RepositoryCache.empty(buildpressCacheFile))
    else {
      val bytes = Files.readAllBytes(buildpressCacheFile.underlying)
      val contents = new String(bytes, StandardCharsets.UTF_8)
      parseUris(buildpressCacheFile.syntax, contents)
        .map(repos => RepositoryCache(buildpressCacheFile, repos))
    }
  }

  def parseAndCloneRepositories(
      params: BuildpressParams,
      buildpressHome: AbsolutePath,
      cachedRepos: RepositoryCache
  ): EitherErrorOr[List[(HashedRepository, AbsolutePath)]] = {
    val buildpressCacheDir = buildpressHome.resolve("cache")
    Files.createDirectories(buildpressCacheDir.underlying)
    val bytes = Files.readAllBytes(params.buildpressFile.underlying)
    val contents = new String(bytes, StandardCharsets.UTF_8)
    parseUris(buildpressCacheDir.syntax, contents).flatMap { uris =>
      traverse(uris) { repo =>
        setUpRepoContents(repo.get, buildpressCacheDir, cachedRepos)
          .map(path => repo -> path)
      }
    }
  }

  def exportRepositories(
      params: BuildpressParams,
      repositoryPaths: List[AbsolutePath]
  ): EitherErrorOr[List[AbsolutePath]] = {
    flatTraverse(repositoryPaths) { buildPath =>
      detectBuildTool(buildPath) match {
        case Some(sbtBuild: BuildTool.Sbt) =>
          out.println(success(s"Detected $sbtBuild"))
          out.println(s"Exporting build to bloop in ${buildPath}...")
          exportSbtBuild(sbtBuild, params.regenerate, params.bloopVersion).flatMap { generated =>
            val bloopDir = sbtBuild.baseDir.resolve(".bloop")
            if (generated && bloopDir.exists) Right(Some(bloopDir))
            else if (!generated) Right(None)
            else {
              val msg = s"Missing $bloopDir after build import!"
              Left(BuildpressError.ImportFailure(error(msg), None))
            }
          }
        case Some(unsupportedBuildTool) =>
          val msg = s"Unsupported build tool $unsupportedBuildTool"
          Left(BuildpressError.ImportFailure(error(msg), None))
        case None =>
          val msg = s"No detected build tool in $buildPath"
          Left(BuildpressError.ImportFailure(error(msg), None))
      }
    }
  }

  def setUpRepoContents(
      repo: Repository,
      cacheDir: AbsolutePath,
      cachedRepos: RepositoryCache
  ): Either[BuildpressError.CloningFailure, AbsolutePath] = {
    if (repo.isLocal) Right(AbsolutePath(repo.uri))
    else {
      if (!repo.supportsGit) {
        val msg = "Expected valid git reference or https to git repo"
        Left(BuildpressError.CloningFailure(error(msg), None))
      } else {
        cloneGitUri(repo, cacheDir, cachedRepos)
      }
    }
  }

  def cloneGitUri(
      repo: Repository,
      cacheDir: AbsolutePath,
      cachedRepos: RepositoryCache
  ): Either[BuildpressError.CloningFailure, AbsolutePath] = {
    repo.sha match {
      case None =>
        val msg =
          s"Missing sha hash in uri ${repo.uri.toASCIIString}, expected format 'git://github.com/foo/repo.git#23063e2813c81daee64d31dd7760f5a4fae392e6'"
        Left(BuildpressError.CloningFailure(error(msg), None))
      case Some(sha) =>
        def clone(cloneTargetDir: AbsolutePath) = {
          val clonePath = cloneTargetDir.underlying
          val cloneUri = repo.uriWithoutSha
          val cloneCmd = List("git", "clone", cloneUri, cloneTargetDir.syntax)
          out.println(s"Cloning ${cloneUri}...")
          shell.runCommand(cloneCmd, cwd.underlying, Some(4 * 60L), Some(out)) match {
            case status if status.isOk =>
              val cloneSubmoduleCmd = List("git", "submodule", "update", "--init")
              out.println(s"Cloning submodules of ${cloneUri}...")
              shell.runCommand(cloneSubmoduleCmd, clonePath, Some(60L), Some(out)) match {
                case finalCloneStatus if finalCloneStatus.isOk =>
                  out.println(success(s"Cloned $cloneUri"))
                  val checkoutCmd = List("git", "checkout", "-q", sha)
                  shell.runCommand(checkoutCmd, clonePath, Some(30L), Some(out)) match {
                    case checkoutStatus if checkoutStatus.isOk => Right(cloneTargetDir)
                    case failedCheckout =>
                      val checkoutMsg =
                        s"Failed to checkout $sha in $cloneTargetDir: $failedCheckout"
                      Left(BuildpressError.CloningFailure(error(checkoutMsg), None))
                  }
                case failedClone =>
                  val cloneErrorMsg = s"Failed to clone submodules of $cloneUri: $failedClone"
                  Left(BuildpressError.CloningFailure(error(cloneErrorMsg), None))
              }

            case failedClone =>
              val cloneErrorMsg = s"Failed to clone $cloneUri in $clonePath: $failedClone"
              Left(BuildpressError.CloningFailure(error(cloneErrorMsg), None))
          }
        }

        def deleteCloneDir(cloneTargetDir: AbsolutePath) = {
          try {
            BuildpressPaths.delete(cloneTargetDir)
            Right(())
          } catch {
            case t: IOException =>
              val msg = s"Failed to delete $cloneTargetDir: '${t.getMessage}'"
              Left(BuildpressError.CloningFailure(error(msg), None))
          }
        }

        val cloneTargetDir = cacheDir.resolve(repo.id)
        if (!cloneTargetDir.exists) {
          clone(cloneTargetDir)
        } else {
          cachedRepos.getCachedRepoFor(repo) match {
            case None =>
              out.println(
                s"Deleting ${cloneTargetDir.syntax}, missing ${repo.uriWithoutSha} in cache file"
              )
              deleteCloneDir(cloneTargetDir)
              clone(cloneTargetDir)

            case Some(oldRepo) =>
              if (oldRepo.get.uri == repo.uri) {
                out.println(s"Skipping git clone for ${repo.id}, ${cloneTargetDir.syntax} exists")
                Right(cloneTargetDir)
              } else {
                out.println(
                  s"Deleting ${cloneTargetDir.syntax}, uri ${repo.uri.toASCIIString} != ${oldRepo.get.uri.toASCIIString}"
                )
                deleteCloneDir(cloneTargetDir)
                clone(cloneTargetDir)
              }
          }
        }
    }
  }

  def parseUris(
      fromPath: String,
      contents: String
  ): Either[BuildpressError.ParseFailure, List[HashedRepository]] = {
    val parseResults = contents.split(System.lineSeparator).zipWithIndex.flatMap {
      case (line, idx) =>
        if (line.startsWith("//")) Nil
        else {
          val lineNumber = idx + 1
          def position = s"$fromPath:$lineNumber"
          line.split(",") match {
            case Array("") => Nil

            case Array() | Array(_) =>
              val msg = s"Missing comma between repo id and repo URI at $position"
              List(Left(BuildpressError.ParseFailure(error(msg), None)))

            case data if data.length <= 3 =>
              val eid = Right(data(0).trim)
              val euri = Try(new URI(data(1).trim)).toEither.left.map { err =>
                val msg = s"Expected URI syntax at $position, obtained [${data(1)}]"
                BuildpressError.ParseFailure(error(msg), Some(err))
              }
              val ehash =
                Try(data.lift(2).map(_.trim.toInt)).toEither.left.map { err =>
                  val msg = s"Expected hash to be an int at $position, got [${data.lift(2)}]"
                  BuildpressError.ParseFailure(error(msg), Some(err))
                }
              val repo = for {
                id <- eid
                uri <- euri
                hash <- ehash
              } yield {
                HashedRepository(Repository(id, uri), hash)
              }
              List(repo)

            case _ =>
              val msg =
                s"Expected buildpress line format 'id,uri[,hash]' at $position, obtained '$line'"
              List(Left(BuildpressError.ParseFailure(error(msg), None)))
          }
        }
    }

    // Report failures to the user, one per one
    val failures = parseResults.collect { case Left(e) => e }
    val failureMsgs = failures.map { e =>
      val error = new StringBuilder()
      error
        .++=(e.msg)
        .++=(System.lineSeparator())
        .++=(e.cause.map(t => s"   Parse error: ${t.getMessage}").getOrElse(""))
        .mkString
    }

    if (failures.nonEmpty) {
      val completeErrorMsg =
        failureMsgs.mkString(System.lineSeparator) +
          System.lineSeparator() +
          error(s"Found ${failures.size} errors when parsing URIs")
      Left(BuildpressError.ParseFailure(completeErrorMsg, None))
    } else {
      val uriEntries = parseResults.collect { case Right(uri) => uri }.toList
      val visitedIds = new mutable.HashMap[String, URI]()
      traverse(uriEntries) {
        case entry @ HashedRepository(Repository(id, uri), _) =>
          visitedIds.get(id) match {
            case Some(alreadyMappedUri) =>
              val msg = s"Id '$id' is already used by URI ${alreadyMappedUri}"
              Left(BuildpressError.ParseFailure(error(msg), None))
            case None =>
              visitedIds.+=(id -> uri)
              Right(entry)
          }
      }
    }
  }

  def detectBuildTool(baseDir: AbsolutePath): Option[BuildTool] = {
    val sbtMetaProject = baseDir.resolve("project")
    if (sbtMetaProject.exists) {
      val sbtProperties = new java.util.Properties()
      val sbtPropertiesFile = sbtMetaProject.resolve("build.properties")
      val inProperties = Files.newInputStream(sbtPropertiesFile.underlying)
      try sbtProperties.load(inProperties)
      finally inProperties.close()
      val sbtVersion = sbtProperties.getProperty("sbt.version")
      Some(BuildTool.Sbt(baseDir, sbtVersion))
    } else {
      // TODO: Support gradle, mill and maven here
      None
    }
  }

  def exportSbtBuild(
      buildTool: BuildTool.Sbt,
      regenerate: Boolean,
      bloopVersion: String
  ): Either[BuildpressError, Boolean] = {
    // TODO: Don't add bloop sbt plugin if build already has set it up
    def addSbtPlugin(buildpressSbtFile: AbsolutePath) = {
      val sbtFileContents =
        s"""addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "$bloopVersion")""".stripMargin
      try {
        val bytes = sbtFileContents.getBytes(StandardCharsets.UTF_8)
        Files.write(buildpressSbtFile.underlying, bytes)
        Right(())
      } catch {
        case NonFatal(t) =>
          val msg = s"Unexpected exception when writing to to $buildpressSbtFile"
          Left(BuildpressError.ImportFailure(error(msg), Some(t)))
      }
    }

    def runBloopInstall(baseDir: AbsolutePath) = {
      // Run bloop install for 15 minutes at maximum per project
      val cmd = List(
        "sbt",
        "-warn",
        "-J-Djline.terminal=jline.UnsupportedTerminal",
        "-J-Dsbt.log.noformat=true",
        "-J-Dfile.encoding=UTF-8",
        "bloopInstall"
      )

      val timeout = Some(15 * 60L) // Maximum wait is 15 minutes
      shell.runCommand(cmd, baseDir.underlying, timeout, Some(out)) match {
        case status if status.isOk => Right(())
        case failed =>
          val msg = s"Unexpected failure when running `${cmd.mkString(" ")}` in $baseDir"
          Left(BuildpressError.ImportFailure(error(msg), None))
      }
    }

    val bloopConfigDir = buildTool.baseDir.resolve(".bloop")
    val metaProjectDir = buildTool.baseDir.resolve("project")
    val buildpressSbtFile = metaProjectDir.resolve("buildpress.sbt")
    if (bloopConfigDir.exists && !regenerate) {
      out.println(success(s"Skipping export, ${buildTool.baseDir} exists"))
      Right(false)
    } else {
      for {
        _ <- addSbtPlugin(buildpressSbtFile)
        _ <- runBloopInstall(buildTool.baseDir)
      } yield {
        out.println(success(s"Exported ${buildTool.baseDir}"))
        true
      }
    }
  }
}
