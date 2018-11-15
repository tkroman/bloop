package bloop.reporter

import bloop.io.AbsolutePath
import bloop.logging.Logger
import xsbti.compile.CompileAnalysis
import xsbti.{Position, Severity}

import ch.epfl.scala.bsp

import scala.collection.mutable

/**
 * A flexible reporter whose configuration is provided by a `ReporterConfig`.
 * This configuration indicated whether to use colors, how to format messages,
 * etc.
 *
 * @param logger The logger that will receive the output of the reporter.
 * @param cwd    The current working directory of the user who started compilation.
 * @param sourcePositionMapper A function that transforms positions.
 * @param config The configuration for this reporter.
 */
abstract class Reporter(
    val logger: Logger,
    val cwd: AbsolutePath,
    sourcePositionMapper: Position => Position,
    val config: ReporterConfig,
    val _problems: mutable.Buffer[Problem] = mutable.ArrayBuffer.empty
) extends xsbti.Reporter
    with ConfigurableReporter {

  private var _nextID = 1
  override def reset(): Unit = { _problems.clear(); _nextID = 1; () }
  private def nextID(): Int = { val id = _nextID; _nextID += 1; id }

  override def hasErrors(): Boolean = hasErrors(_problems)
  override def hasWarnings(): Boolean = hasWarnings(_problems)

  override def allProblems: Seq[Problem] = _problems
  override def problems(): Array[xsbti.Problem] = _problems.toArray

  protected def logFull(problem: Problem): Unit

  override def log(prob: xsbti.Problem): Unit = {
    val mappedPos = sourcePositionMapper(prob.position)
    val problemID = if (prob.position.sourceFile.isPresent) nextID() else -1
    val problem =
      Problem(problemID, prob.severity, prob.message, mappedPos, prob.category)
    _problems += problem

    // If we show errors in reverse order, they'll all be shown
    // in `printSummary`.
    if (!config.reverseOrder) {
      logFull(problem)
    }
  }

  override def comment(pos: Position, msg: String): Unit = ()

  private def hasErrors(problems: Seq[Problem]): Boolean =
    problems.exists(_.severity == Severity.Error)

  private def hasWarnings(problems: Seq[Problem]): Boolean =
    problems.exists(_.severity == Severity.Warn)

  def reportStartCompilation(): Unit
  def reportEndCompilation(code: bsp.StatusCode): Unit
}

final class LogReporter(
    override val logger: Logger,
    override val cwd: AbsolutePath,
    sourcePositionMapper: Position => Position,
    override val config: ReporterConfig,
    override val _problems: mutable.Buffer[Problem] = mutable.ArrayBuffer.empty
) extends Reporter(logger, cwd, sourcePositionMapper, config, _problems) {

  private final val format = config.format(this)
  override def printSummary(): Unit = {
    if (config.reverseOrder) { _problems.reverse.foreach(logFull) }
    format.printSummary()
  }

  /**
   * Log the full error message for `problem`.
   *
   * @param problem The problem to log.
   */
  override protected def logFull(problem: Problem): Unit = {
    val text = format.formatProblem(problem)
    problem.severity match {
      case Severity.Error => logger.error(text)
      case Severity.Warn => logger.warn(text)
      case Severity.Info => logger.info(text)
    }
  }

  override def reportStartCompilation(): Unit = ()
  override def reportEndCompilation(code: bsp.StatusCode): Unit = ()
}

object Reporter {
  def fromAnalysis(analysis: CompileAnalysis, cwd: AbsolutePath, logger: Logger): Reporter = {
    import scala.collection.JavaConverters._
    val sourceInfos = analysis.readSourceInfos.getAllSourceInfos.asScala.toBuffer
    val ps = sourceInfos.flatMap(_._2.getReportedProblems).map(Problem.fromZincProblem(_))
    new LogReporter(logger, cwd, identity, ReporterConfig.defaultFormat, ps)
  }
}
