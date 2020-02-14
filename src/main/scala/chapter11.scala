package tkngch.exercises_fpinscala.chapter11

import tkngch.exercises_fpinscala.chapter05.Stream
import tkngch.exercises_fpinscala.chapter07.Par
import tkngch.exercises_fpinscala.chapter07.Par.Par
import tkngch.exercises_fpinscala.chapter08.Gen
import tkngch.exercises_fpinscala.chapter09.Parsers

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /*
   * Ex 11.3 Implement sequence and traverse for Monad[F]
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma match {
      case h :: t => map2(h, sequence(t))(_ :: _)
      case Nil    => unit(List.empty)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  /*
   * Ex 11.4 Implement replicateM.
   */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /*
   * Ex 11.6 Implement filterM.
   */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case h :: t =>
        flatMap(f(h))(x => if (x) map(filterM(t)(f))(h :: _) else filterM(t)(f))
      case Nil => unit(List.empty)
    }

  /*
   * Ex 11.7 Implement the Kleisli composition function.
   */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(flatMap(unit(a))(f))(g)

  /*
   * Ex 11.8 Implement flatMap in terms of compose.
   */
  def flatMapC[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Option[Int]) => ma, f)(None)

  /*
   * Ex 11.12 Implement join in terms of flatMap.
   */
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(x => x)

  /*
   * Ex 11.13 Implement either flatMap or compose in terms of join and map.
   */
  def flatMapJ[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def composeJ[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  /*
   * Ex 11.1 Write monad instances for Par, Parser, Option, Stream, and List.
   */
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

}

/*
 * Ex 11.17 Implement map and flatMap as a method on Id class and give an
 * implementation for Monad[Id].
 */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] =
    Id(f(value))

  def flatMap[B](f: A => Id[B]) =
    f(value)
}
object idMonad extends Monad[Id] {
  def unit[A](a: => A) = Id(a)
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
}
