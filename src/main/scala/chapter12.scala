package tkngch.exercises_fpinscala.chapter12

import tkngch.exercises_fpinscala.chapter06.State
import tkngch.exercises_fpinscala.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /*
   * Ex 12.1 Transplant the implementations of combinators from Monad to Applicative,
   * using only map2 and unit, or methods implemented in terms of them.
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(x => x)
  // fas.foldRight(unit(List.empty: List[A]))((fa, b) =>
  //   map2(fa, b)((a, b) => (a :: b))
  // )

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  /*
   * Ex 12.2 Implement apply in terms of map2 and unit; and implement map and map2 in
   * terms of apply and unit.
   */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def mapA[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2A[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    apply(mapA(ma)(f.curried))(mb)

  /*
   * Ex 12.3 Implement map3 and map4 using only unit, apply, and the curried method
   * available on functions.
   */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  /*
   * Ex 12.8 Implement the product of two applicative functors.
   */
  def product[G[_]](
      g: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))

      def map2[A, B, C](ma: (F[A], G[A]), mb: (F[B], G[B]))(
          f: (A, B) => C
      ): (F[C], G[C]) = (self.map2(ma._1, mb._1)(f), g.map2(ma._2, mb._2)(f))

    }
  }

  /*
   * Ex 12.9 Implement compose function.
   */
  def compose[G[_]](
      g: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))

      def map2[A, B, C](ma: F[G[A]], mb: F[G[B]])(
          f: (A, B) => C
      ): F[G[C]] = self.map2(ma, mb)((ga, gb) => g.map2(ga, gb)(f))

    }
  }

  /*
   * Ex 12.12 Implement sequence over a Map.
   */
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map(): Map[K, V]))({
      case (b, (k, fv)) => map2(fv, b)((v, m) => m.updated(k, v))
    })
}

object Applicative {}

trait Monad[F[_]] extends Applicative[F] {
  // Implementation of Monad must provide flatMap, or join and map.
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

}

object MonadInstances {
  /*
   * Ex 12.5 Write a monad instance for Either.
   */
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](
          eea: Either[E, A]
      )(f: A => Either[E, B]): Either[E, B] =
        eea match {
          case Right(a) => f(a)
          case Left(e)  => Left(e)
        }

    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](
        st: State[S, A]
    )(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }

}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object ApplicativeInstances {
  /*
   * Ex 12.6 Write an Applicative instance for Validation that accumulates errors in
   * Failure.
   */
  def validationApplicative[E]
      : Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      def map2[A, B, C](ma: Validation[E, A], mb: Validation[E, B])(
          f: (A, B) => C
      ): Validation[E, C] =
        (ma, mb) match {
          case (Success(a), Success(b))    => Success(f(a, b))
          case (Success(a), Failure(h, t)) => Failure(h, t)
          case (Failure(h, t), Success(b)) => Failure(h, t)
          case (Failure(h, t), Failure(h2, t2)) =>
            Failure(h, t ++ Vector(h2) ++ t2)
        }
    }
}

trait Traverse[F[_]] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  /*
   * Ex 12.14 Implement map in terms of traverse.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(
      MonadInstances.stateMonad
    )

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      State
        .get[S]
        .flatMap(s1 => {
          val (b, s2) = f(a, s1)
          State.set(s2).map(_ => b)
        })
    ).run(s)

  def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) =>
      State.get[List[A]].flatMap(as => State.set(a :: as).map(_ => ()))
    ).run(Nil)._2.reverse
  // For comprehension version:
  // traverseS(fa)((a: A) =>
  //   (for {
  //     as <- State.get[List[A]]
  //     _ <- State.set(a :: as)
  //   } yield ())
  // ).run(Nil)._2.reverse

  /*
   * Ex 12.16 Write a function to reverse any traversable functor.
   */
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

  /*
   * Ex 12.17 Use mapAccum to give a default implementation of foldLeft for the Traverse
   * trait.
   */
  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  /*
   * Ex 12.18 Use applicative functor products to write the fusion of two traversals.
   */
  def fuse[G[_], H[_], A, B](fa: F[A])(
      f: A => G[B],
      g: A => H[B]
  )(implicit ag: Applicative[G], ah: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(
      ag.product(ah)
    )
}

object TraverseInstances {
  /*
   * Ex 12.13 Write Traverse instances of List, Option, and Tree.
   */
  def listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](
        fa: List[A]
    )(f: A => G[B])(implicit g: Applicative[G]): G[List[B]] =
      fa.foldRight(g.unit(List.empty: List[B]))((a, gb) =>
        g.map2(f(a), gb)(_ :: _)
      )
  }

  def optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](
        fa: Option[A]
    )(f: A => G[B])(implicit g: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(x) => g.map(f(x))(a => Some(a))
        case None    => g.unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])
  def treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](
        fa: Tree[A]
    )(f: A => G[B])(implicit g: Applicative[G]): G[Tree[B]] =
      fa match {
        case Tree(head, tail) =>
          g.map2(
            f(head),
            tail.foldRight(g.unit(List.empty: List[Tree[B]]))((a, gb) =>
              g.map2(traverse(a)(f), gb)(_ :: _)
            )
          )((x, y) => Tree(x, y))
      }
  }

  /*
   * Ex 12.19 Implement the composition of two Traverse instances.
   */
  def compose[F[_], G[_]](
      implicit tf: Traverse[F],
      tg: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] =
    new Traverse[({ type f[x] = F[G[x]] })#f] {
      override def traverse[H[_]: Applicative, A, B](
          fga: F[G[A]]
      )(f: A => H[B]) =
        tf.traverse(fga)((ga: G[A]) => tg.traverse(ga)(f))
    }

}
