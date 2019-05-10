package buildpress.util

import scala.collection.mutable

object Traverse {
  object either {
    def traverse[A, L, R](xs: List[A])(f: A => Either[L, R]): Either[L, List[R]] = {
      // O(n) best/worst case
      xs.foldLeft(Right(List.newBuilder[R]): Either[L, mutable.Builder[R, List[R]]]) {
          case (l @ Left(_), _) => l
          case (Right(acc), x) => f(x).map(r => acc += r)
        }
        .map(_.result)
    }

    def flatTraverse[A, L, R](xs: List[A])(f: A => Either[L, Iterable[R]]): Either[L, List[R]] = {
      // O(n) best/worst case
      xs.foldLeft(Right(List.newBuilder[R]): Either[L, mutable.Builder[R, List[R]]]) {
          case (l @ Left(_), _) => l
          case (Right(acc), x) => f(x).map(r => acc ++= r)
        }
        .map(_.result)
    }
  }
}
