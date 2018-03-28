package bloop

import java.nio.file.{Files, Paths => NioPaths}
import java.util.Properties

import bloop.exec.JavaEnv
import bloop.io.{AbsolutePath, Paths}
import bloop.io.Timer.timed
import bloop.logging.Logger
import xsbti.compile.ClasspathOptions

import scala.util.control.NoStackTrace

case class Project(name: String,
                   baseDirectory: AbsolutePath,
                   dependencies: Array[String],
                   scalaInstance: ScalaInstance,
                   rawClasspath: Array[AbsolutePath],
                   classpathOptions: ClasspathOptions,
                   classesDir: AbsolutePath,
                   scalacOptions: Array[String],
                   javacOptions: Array[String],
                   sourceDirectories: Array[AbsolutePath],
                   testFrameworks: Array[Array[String]],
                   javaEnv: JavaEnv,
                   tmp: AbsolutePath,
                   bloopConfigDir: AbsolutePath) {
  override def toString: String = s"$name"

  /** This project's full classpath (classes directory and raw classpath) */
  val classpath: Array[AbsolutePath] = {
    classesDir +: rawClasspath
  }
}

object Project {

  /** The pattern used to find configuration files */
  final val loadPattern: String = "glob:**.config"

  /** The maximum number of directory levels to traverse to find configuration files. */
  final val loadDepth: Int = 1

  /**
   * Load all the projects from `config`.
   *
   * @param config The base directory from which to load the projects.
   * @param logger The logger that collects messages about project loading.
   * @return The list of loaded projects.
   */
  def fromDir(config: AbsolutePath, logger: Logger): List[Project] = {
    timed(logger) {
      // TODO: We're not handling projects with duplicated names here.
      val configFiles = Paths.getAll(config, loadPattern, maxDepth = loadDepth)
      logger.debug(s"Loading ${configFiles.length} projects from '${config.syntax}'...")
      configFiles.par.map(configFile => fromFile(configFile, logger)).toList
    }
  }

  private[bloop] def fromFile(config: AbsolutePath, logger: Logger): Project = {
    logger.debug(s"Loading project from '$config'")
    val configFilepath = config.underlying
    val properties = new Properties()
    val inputStream = Files.newInputStream(configFilepath)
    try properties.load(inputStream)
    finally inputStream.close
    fromProperties(properties, config, logger)
  }

  private class MissingFieldError(fieldName: String, config: AbsolutePath)
      extends Exception(
        s"""The field '$fieldName' is missing in '${config.syntax}'.
           |Please export your project again from your build tool (e.g. `bloopInstall`).
           |Check the installation page for further information: https://scalacenter.github.io/bloop/docs/installation""".stripMargin
      ) with NoStackTrace

  def fromProperties(properties: Properties, config: AbsolutePath, logger: Logger): Project = {
    def toPaths(line: String) = line.split(",").map(toPath)
    def toPath(line: String) = AbsolutePath(NioPaths.get(line))
    val name = properties.getProperty("name")
    val baseDirectory = toPath(properties.getProperty("baseDirectory"))
    val dependencies =
      properties.getProperty("dependencies").split(",").filterNot(_.isEmpty)
    val scalaOrganization = properties.getProperty("scalaOrganization")
    val allScalaJars = toPaths(properties.getProperty("allScalaJars"))
    val scalaName = properties.getProperty("scalaName")
    val scalaVersion = properties.getProperty("scalaVersion")
    val scalaInstance =
      ScalaInstance(scalaOrganization, scalaName, scalaVersion, allScalaJars, logger)
    val classpath = toPaths(properties.getProperty("classpath"))
    val classesDir = toPath(properties.getProperty("classesDir"))
    val classpathOptions = {
      val opts = properties.getProperty("classpathOptions")
      // Protecting our users from breaking changes in the configuration file format.
      if (opts == null) throw new MissingFieldError("classpathOptions", config)
      else {
        val values = opts.split(",")
        val Array(bootLibrary, compiler, extra, autoBoot, filterLibrary) =
          values.map(java.lang.Boolean.parseBoolean)
        ClasspathOptions.of(bootLibrary, compiler, extra, autoBoot, filterLibrary)
      }
    }
    val scalacOptions =
      properties.getProperty("scalacOptions").split(";").filterNot(_.isEmpty)
    val javacOptions =
      properties.getProperty("javacOptions").split(";").filterNot(_.isEmpty)
    val sourceDirectories = properties
      .getProperty("sourceDirectories")
      .split(",")
      .filterNot(_.isEmpty)
      .map(toPath)
    val testFrameworks =
      properties.getProperty("testFrameworks").split(";").map(_.split(",").filterNot(_.isEmpty))
    val javaHome = toPath(properties.getProperty("javaHome"))
    val javaOptions = properties.getProperty("javaOptions").split(";").filterNot(_.isEmpty)
    val javaEnv = JavaEnv(javaHome, javaOptions)
    val tmp = AbsolutePath(NioPaths.get(properties.getProperty("tmp")))
    Project(
      name,
      baseDirectory,
      dependencies,
      scalaInstance,
      classpath,
      classpathOptions,
      classesDir,
      scalacOptions,
      javacOptions,
      sourceDirectories,
      testFrameworks,
      javaEnv,
      tmp,
      config
    )
  }
}
