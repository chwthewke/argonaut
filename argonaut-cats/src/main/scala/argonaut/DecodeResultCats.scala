package argonaut

import CursorHistoryCats._
import cats._, data._
import ext.std.tuple._
import instances.either._, instances.string._
import scala.annotation.tailrec
import syntax.contravariant._

object DecodeResultCats extends DecodeResultCatss {
}

trait DecodeResultCatss {

  @annotation.tailrec
  final def loop[A, X](d: DecodeResult[A], e: (String, CursorHistory) => X, f: A => Xor[X, DecodeResult[A]]): X = {
    if (d.isError) {
      e(d.message.get, d.history.get)
    } else {
      f(d.value.get) match {
        case Xor.Left(x) => x
        case Xor.Right(a) => loop(a, e, f)
      }
    }
  }

  type DecodeEither[A] = Either[(String, CursorHistory), A]

  implicit def DecodeResultEq[A](implicit EA: Eq[A]): Eq[DecodeResult[A]] =
    catsStdEqForEither[(String, CursorHistory), A].on(_.toEither)

  implicit def DecodeResultMonad: Monad[DecodeResult] with RecursiveTailRecM[DecodeResult] =
    new Monad[DecodeResult] with RecursiveTailRecM[DecodeResult] {
      def pure[A](a: A) = DecodeResult.ok(a)
      def flatMap[A, B](a: DecodeResult[A])(f: A => DecodeResult[B]) = a flatMap f
      override def map[A, B](a: DecodeResult[A])(f: A => B) = a map f

      @tailrec
      override def tailRecM[A, B](a: A)(fn : A => DecodeResult[Either[A, B]]): DecodeResult[B] =
        fn(a).result match {
          case Left(err)       => DecodeResult(Left(err))
          case Right(Left(a1)) => tailRecM(a1)(fn)
          case Right(Right(b)) => DecodeResult(Right(b))
        }
    }

  implicit def DecodeResultShow[A](implicit SE: Show[DecodeEither[A]]): Show[DecodeResult[A]] =
    SE.contramap(_.toEither)
}
