package bloop.engine.tasks.compilation

import java.net.URI
import java.util.Optional
import java.util.concurrent.CompletableFuture

import bloop.{Compiler, JavaSignal}
import bloop.reporter.Problem
import bloop.util.CacheHashCode
import monix.eval.Task

import scala.util.Try

sealed trait CompileResult[+R] {
  def result: R
}

sealed trait PartialCompileResult extends CompileResult[Task[Compiler.Result]] {
  def result: Task[Compiler.Result]
}

object PartialCompileResult {
  def apply(
      bundle: CompileBundle,
      pickleURI: Try[Optional[URI]],
      completeJava: CompletableFuture[Unit],
      javaTrigger: Task[JavaSignal],
      result: Task[Compiler.Result]
  ): PartialCompileResult = {
    pickleURI match {
      case scala.util.Success(opt) =>
        PartialSuccess(bundle, opt, completeJava, javaTrigger, result)
      case scala.util.Failure(CompileExceptions.CompletePromise) =>
        PartialSuccess(bundle, Optional.empty(), completeJava, javaTrigger, result)
      case scala.util.Failure(t) =>
        PartialFailure(bundle, t, result)
    }
  }

  def toFinalResult(result: PartialCompileResult): Task[List[FinalCompileResult]] = {
    result match {
      case PartialEmpty => Task.now(FinalEmptyResult :: Nil)
      case PartialFailure(project, _, result) =>
        result.map(res => FinalNormalCompileResult(project, res) :: Nil)
      case PartialFailures(failures, result) =>
        Task.gatherUnordered(failures.map(toFinalResult(_))).map(_.flatten)
      case PartialSuccess(project, _, _, _, result) =>
        result.map(res => FinalNormalCompileResult(project, res) :: Nil)
    }
  }
}

case object PartialEmpty extends PartialCompileResult {
  override final val result: Task[Compiler.Result] = Task.now(Compiler.Result.Empty)
}

case class PartialFailure(
    bundle: CompileBundle,
    exception: Throwable,
    result: Task[Compiler.Result]
) extends PartialCompileResult
    with CacheHashCode

case class PartialFailures(
    failures: List[PartialCompileResult],
    result: Task[Compiler.Result]
) extends PartialCompileResult
    with CacheHashCode

case class PartialSuccess(
    bundle: CompileBundle,
    pickleURI: Optional[URI],
    completeJava: CompletableFuture[Unit],
    javaTrigger: Task[JavaSignal],
    result: Task[Compiler.Result]
) extends PartialCompileResult
    with CacheHashCode

sealed trait FinalCompileResult extends CompileResult[Compiler.Result] {
  def result: Compiler.Result
}

case object FinalEmptyResult extends FinalCompileResult {
  final val result = Compiler.Result.Empty
}

case class FinalNormalCompileResult private (
    bundle: CompileBundle,
    result: Compiler.Result
) extends FinalCompileResult
    with CacheHashCode

object FinalCompileResult {
  import scalaz.Show
  final implicit val showFinalResult: Show[FinalCompileResult] = new Show[FinalCompileResult] {
    private def seconds(ms: Double): String = s"${ms}ms"
    override def shows(r: FinalCompileResult): String = {
      r match {
        case FinalEmptyResult => s"<empty> (product of dag aggregation)"
        case FinalNormalCompileResult(bundle, result) =>
          val projectName = bundle.project.name
          result match {
            case Compiler.Result.Empty => s"${projectName} (empty)"
            case Compiler.Result.Cancelled(ms) => s"${projectName} (cancelled, lasted ${ms}ms)"
            case Compiler.Result.Success(_, _, ms) => s"${projectName} (success ${ms}ms)"
            case Compiler.Result.Blocked(on) => s"${projectName} (blocked on ${on.mkString(", ")})"
            case Compiler.Result.GlobalError(problem) =>
              s"${projectName} (failed with global error ${problem})"
            case Compiler.Result.Failed(problems, t, ms) =>
              val extra = t match {
                case Some(t) => s"exception '${t.getMessage}', "
                case None => ""
              }
              s"${projectName} (failed with ${Problem.count(problems)}, $extra${ms}ms)"
          }
      }
    }
  }
}
