package bloop.reporter

import bloop.data.Project
import bloop.io.AbsolutePath
import bloop.logging.BspServerLogger
import xsbti.Position

import ch.epfl.scala.bsp

import scala.collection.mutable

final class BspProjectReporter(
    val project: Project,
    override val logger: BspServerLogger,
    override val cwd: AbsolutePath,
    sourcePositionMapper: Position => Position,
    override val config: ReporterConfig,
    override val _problems: mutable.Buffer[Problem] = mutable.ArrayBuffer.empty
) extends Reporter(logger, cwd, sourcePositionMapper, config, _problems) {
  private val taskId = logger.nextTaskId
  override protected def logFull(problem: Problem): Unit =
    logger.diagnostic(project, problem)

  // Report summary manually via `reportEndCompilation` for BSP clients
  override def printSummary(): Unit = ()

  override def reportStartCompilation(): Unit = {
    logger.publishCompileStart(project, taskId)
  }

  override def reportEndCompilation(code: bsp.StatusCode): Unit = {
    logger.publishCompileEnd(project, taskId, allProblems, code)
  }
}
