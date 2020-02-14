package tkngch.exercises_fpinscala.chapter13

import tkngch.exercises_fpinscala.chapter12.Monad

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
    extends Free[F, B]

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap((x: A) => Return(f(x)))
}

object MonadInstances {
  /*
   * Ex 13.1 Free is a monad for any choice of F. Implement map and flatMap methods on
   * the Free trait, and give Monad instance for Free[F, _].
   */
  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f] {
      def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](
          a: Free[F, A]
      )(f: A => Free[F, B]): Free[F, B] =
        a.flatMap(f)
    }
}

object FreeOps {
  /*
   * Ex 13.2 Implement runTrampoline for running Free[Function0, A].
   */
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A =
    a match {
      case Return(x)  => x
      case Suspend(x) => x()
      case FlatMap(x, f) =>
        x match {
          case Return(y)     => runTrampoline(f(y))
          case Suspend(y)    => runTrampoline(f(y()))
          case FlatMap(y, g) => runTrampoline(y.flatMap(z => g(z).flatMap(f)))
        }
    }

  /*
   * Ex 13.3 Implement a generic interpreter for Free[F, A].
   */
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] =
    a match {
      case FlatMap(FlatMap(y, g), f) => y.flatMap(z => g(z).flatMap(f))
      case FlatMap(Return(y), f)     => step(f(y))
      case _                         => a
    }

  def run[F[_], A](a: Free[F, A])(implicit m: Monad[F]): F[A] =
    step(a) match {
      case Return(x)  => m.unit(x)
      case Suspend(x) => x
      case FlatMap(x, f) =>
        x match {
          case Suspend(y) => m.flatMap(y)(z => run(f(z)))
          case _          => sys.error("error")
        }
    }
}
