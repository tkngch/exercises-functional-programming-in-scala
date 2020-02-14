package tkngch.exercises_fpinscala.chapter10

import tkngch.exercises_fpinscala.chapter05.{Stream, Cons, Empty}
import tkngch.exercises_fpinscala.chapter08.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object MonoidOps {
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /*
   * Ex 10.5 Implement foldMap.
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  /*
   * Ex 10.4 Write foldRight and foldLeft using foldMap.
   */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, MonoidInstances.endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, MonoidInstances.dual(MonoidInstances.endoMonoid[B]))(a =>
      b => f(b, a)
    )(z)

  /*
   * Ex 10.7 Implement a foldMap for IndexedSeq. Your implementation should use the
   * strategy of splitting the sequence in two, recursively processing each half, and
   * then adding the answers together with the monoid.
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v.length match {
      case 0 => m.zero
      case 1 => f(v(0))
      case _ =>
        val vs = v.splitAt(v.length / 2)
        m.op(foldMapV(vs._1, m)(f), foldMapV(vs._2, m)(f))
    }

  /*
   * Ex 10.8 Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
   */
  def isOrdered(a: IndexedSeq[Int]): Boolean =
    foldMapV(a, MonoidInstances.intOrdered)(x => Some((x, x, true))).get._3

  /*
   * Ex 10.11 Use the WC monoid to implement a function that counts words in a String.
   */
  def countWords(s: String): Int = {
    def count(sc: String): WC = {
      foldMapV(sc.toIndexedSeq, MonoidInstances.wcMonoid)(x =>
        if (x.isWhitespace) Part("", 0, "") else Stub(x.toString)
      )
    }
    count(s) match {
      case Stub(x) => if (x.isEmpty) 0 else 1
      case Part(l, c, r) =>
        c + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
    }
  }

  /*
   * Ex 10.18 Use monoids to compute a "bag" from an IndexedSeq.
   */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m = MonoidInstances.mapMergeMonoid[A, Int](MonoidInstances.intAddition)
    as.map(x => Map.apply((x, 1))).foldLeft(m.zero)(m.op(_, _))
  }

}

object MonoidInstances {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  /*
   * Ex 10.1 Give Monoid instances for integer addition and multiplication as well as
   * the Boolean operators.
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  /*
   * Ex 10.2 Give a Monoid instance for combining Option values.
   */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = if (a1.isDefined) a1 else a2
    val zero = None
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  /*
   * Ex 10.3 Write a monoid for endofunctions.
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1.andThen(a2)
    val zero = x => x
  }

  /*
   * Ex 10.4 Implement a property for the monoid laws.
   */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    // Associativity
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)
    ) &&
      // Identity
      Prop.forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  val intOrdered: Monoid[Option[(Int, Int, Boolean)]] =
    new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((min1, max1, sorted1)), Some((min2, max2, sorted2))) =>
            Some(
              (
                min1.min(min2),
                max1.max(max2),
                sorted1 && sorted2 && max1 <= min2
              )
            )
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }

  /*
   * Ex 10.10 Write a monoid instance for WC.
   */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Part(ls1, w1, rs1), Part(ls2, w2, rs2)) =>
        Part(ls1, w1 + w2 + (if ((rs1 + ls2).isEmpty) 0 else 1), rs2)
      case (Stub(c1), Part(ls2, w2, rs2)) => Part(c1 + ls2, w2, rs2)
      case (Part(ls1, w1, rs1), Stub(c2)) => Part(ls1, w1, rs1 + c2)
    }
    val zero = Stub("")
  }

  /*
   * Ex 10.16 Prove that if types A and B are monoid, then the tuple type (A, B) is also
   * a monoid.
   */
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) =
        (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
      def zero = (a.zero, b.zero)
    }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero)({ (acc, k) =>
          acc.updated(k, v.op(a.getOrElse(k, v.zero), b.getOrElse(k, v.zero)))
        })
    }

  /*
   * Ex 10.17 Write a monoid instance for functions whose results are monoid.
   */
  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(x: A => B, y: A => B): A => B = a => b.op(x(a), y(a))
    def zero = a => b.zero
  }

}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  /*
   * Ex 10.15 Any Foldable structure can be turned into a List. Write this conversion in
   * a generic way.
   */
  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty: List[A])(_ :: _)

}

object FoldableInstances {
  /*
   * Ex 10.12 Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
   */

  val listFoldable: Foldable[List] = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as match {
        case h :: t => f(h, foldRight(t)(z)(f))
        case Nil    => z
      }

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as match {
        case h :: t => f(foldLeft(t)(z)(f), h)
        case Nil    => z
      }

    def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
      as.map(f).foldRight(m.zero)(m.op)
  }

  val seqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      MonoidOps.foldMapV(as, mb)(f)
  }

  val streamFoldable: Foldable[Stream] = new Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Cons(h, t) => f(h(), foldRight(t())(z)(f))
        case Empty      => z
      }

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Cons(h, t) => f(foldLeft(t())(z)(f), h())
        case Empty      => z
      }

    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  }

  /*
   * Ex 10.13 Implement a Foldable instance for Tree.
   */
  val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(x)      => f(x, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(x)      => f(z, x)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }

    def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
      foldRight(as)(m.zero)((x, y) => m.op(f(x), y))
  }

  /*
   * Ex 10.14 Write a Foldable[Option] instance.
   */
  val optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Some(x) => f(x, z)
        case None    => z
      }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Some(x) => f(z, x)
        case None    => z
      }

    def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
      foldRight(as)(m.zero)((a, b) => m.op(f(a), b))
  }

}
