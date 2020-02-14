package tkngch.exercises_fpinscala.chapter05

import scala.{Option, Some, None}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def isEmpty: Boolean = this match {
    case Cons(h, t) => false
    case Empty      => true
  }

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty      => None
  }

  def head: () => A = this match {
    case Cons(h, t) => h
    case Empty      => ???
  }

  def tail: () => Stream[A] = this match {
    case Cons(h, t) => t
    case Empty      => () => Empty
  }

  /*
   * 5.1: Write a function to convert a Stream to a List, which will force its
   *   evaluation.
   */
  def toList: List[A] = this match {
    case Cons(h, t) => {
      lazy val tl = t()
      if (tl.isEmpty) List(h())
      else
        h() :: Cons(tl.head, tl.tail).toList
    }
    case Empty => List.empty
  }

  /*
   * 5.2: Write the function `take(n)` for returning the first n elements of a Stream,
   *   and `drop(n)` for skipping the first n elements of a Stream.
   */
  def take52(n: Int): Stream[A] = n match {
    case 0 => Empty
    case _ =>
      this match {
        case Cons(h, t) => Cons(h, () => t().take52(n - 1))
        case Empty      => Empty
      }
  }

  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case _ =>
      this match {
        case Cons(h, t) => {
          lazy val tl = t()
          if (tl.isEmpty) Empty else Cons(tl.head, tl.tail).drop(n - 1)
        }
        case Empty => Empty
      }
  }

  /*
   * 5.3: Write the function `takeWhile` for returning all starting elements of a Stream
   *   that match the given predicate.
   */
  def takeWhile53(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile53(p)) else Empty
    case Empty      => Empty
  }

  /*
   * 5.4: Implement `forAll`, which checks that all elements in the Stream match a give
   *   predicate. Your implementation should terminate the traversal as soon as it
   *   encounters a nonmatching value.
   */
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Empty      => println("// forAll reached the end."); true
  }

  /*
   * 5.5: Use `foldRight` to implement `takeWhile`.
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty      => println("// foldRight reached the end."); z
  }

  def takeWhileF(p: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Cons(() => a, () => b) else Empty
    )

  /*
   * 5.6: Implement `headOption` using `foldRight`.
   */
  def headOptionF: Option[A] =
    this.foldRight(None: Option[A])((a, b) => Some(a))

  /*
   * 5.7: Implement map, filter, append, and flatMap using foldRight. The append method
   *   should be non-strict in its argument.
   */
  def map57[B](f: A => B): Stream[B] =
    this.foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Empty: Stream[A])((a, b) =>
      if (f(a)) Cons(() => a, () => b) else b
    )

  def append[B >: A](v: Stream[B]): Stream[B] =
    this.foldRight(v)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  /*
   * 5.13 Use `unfold` to implement map, take, takeWhile, zipWith, and zipAll. The
   *   zipAll function should continue the traversal as long as either stream has more
   *   elements -- it uses Option to indicate whether each stream has been exhausted.
   */
  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s =>
      s match {
        case Cons(h, t) => Some((f(h()), t()))
        case Empty      => None
      }
    )

  def take(n: Int): Stream[A] =
    Stream.unfold((n, this))(s =>
      s match {
        case (i, Cons(h, t)) => if (i > 0) Some((h(), (i - 1, t()))) else None
        case (i, Empty)      => None
      }
    )

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(s =>
      s match {
        case Cons(h, t) => if (p(h())) Some((h(), t())) else None
        case Empty      => None
      }
    )

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2))(s =>
      s match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _                            => None
      }
    )

  def zipAll[B](
      s2: Stream[B]
  ): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2))(s =>
      s match {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
        case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
        case _                     => None
      }
    )

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /*
   * 5.8: Generalise `ones` slightly to the function `constant`, which returns an
   *   infinite Stream of a given value.
   */
  def constant58[A](x: A): Stream[A] = cons(x, constant58(x))

  /*
   * 5.9 Write a function that generates an infinite stream of integers, starting from
   *   n, than n + 1, n + 2, and so on.
   */
  def from59(n: Int): Stream[Int] = cons(n, from59(n + 1))

  /*
   * 5.10: Write a function fibs that generates the infinite stream of Fibonacci numbers.
   */
  def fibs(a: Int = 0, b: Int = 1): Stream[Int] =
    cons(a, fibs(b, a + b))

  /*
   * 5.11 Write a more general stream-building function called `unfold`. It takes an
   *   initial state, and a function for producing both the next state and the next
   *   value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None         => Stream.empty
    }

  /*
   * 5.12 Write fibs, from, constant, and ones in terms of unfold.
   */
  def fibs(): Stream[Int] =
    unfold(List(0, 1))(s =>
      s match {
        case h :: t :: r => Some((h, List(t, h + t)))
        case _           => None
      }
    )

  def from(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def constant[A](x: A): Stream[A] = unfold(x)(s => Some((s, s)))

  /*
   * 5.14 Implement `startsWith` using functions you've written. It should check if one
   * stream is a prefix of another.
   */
  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.zipWith(s2)((a, b) => a == b).foldRight(true)((a, b) => a && b)

  /*
   * 5.15 Implement tails using unfold. FOr a given Stream, tails returns the Stream of
   *   suffixes of the input sequence, starting with the original Stream. For example,
   *   given Stream(1, 2, 3), it would return Stream(Stream(1, 2, 3), Stream(2, 3),
   *   Stream(3), Stream()).
   */
  def tails[A](s: Stream[A]): Stream[Stream[A]] =
    unfold(s)(x =>
      x match {
        case Cons(h, t) => Some((x, t()))
        case Empty      => None
      }
    ).append(Stream(Empty))
}

object Main extends App {
  println("# Exercise 5.1")
  val ss = Stream(1, 2, 3, 4, 5)
  val sl = ss.toList
  println(s"A stream is converted into $sl.")

  println("# Exercise 5.2")
  val x52a = ss.take52(2).toList
  println(s"The stream's first 2 elements is $x52a.")
  val x52b = ss.take52(8).toList
  println(s"The stream's first 8 elements is $x52b.")

  val x52c = ss.drop(2).toList
  println(s"After dropping the first 2 elements, the stream is $x52c.")
  val x52d = ss.drop(8).toList
  println(s"After dropping the first 8 elements, the stream is $x52d.")

  println("# Exercise 5.3")
  val x53a = ss.takeWhile53(_ < 4).toList
  println(s"The starting elements that are less than 4 are $x53a.")

  println("# Exercise 5.4")
  val x54a = ss.forAll(_ < 4)
  println(s"Are all elements in $sl less than 4: $x54a.")
  val x54b = ss.forAll(_ > 0)
  println(s"Are all elements in $sl greater than 0: $x54b.")

  println("# Exercise 5.5")
  val x55a = ss.takeWhileF(_ < 4).toList
  println(
    s"With foldRight, the starting elements that are less than 4 are $x55a."
  )

  println("# Exercise 5.6")
  val x56a = ss.headOptionF
  println(s"With foldRight, the head of $sl is $x56a.")

  println("# Exercise 5.7")
  val x57a = ss.map57(_ * 10).toList
  println(
    s"With map using foldRight, each element in $sl is multipled by 10 to produce $x57a."
  )
  val x57b = ss.filter(_ % 2 == 0).toList
  println(
    s"With filter using foldRight, the odd numbers in $sl are removed to produce $x57b."
  )
  val x57c = Stream(10, 20)
  val x57d = x57c.toList
  val x57e = ss.append(x57c).toList
  println(s"After appending $x57d, $sl becomes $x57e.")

  val x57f = ss.flatMap(x => Stream.cons(x, Stream.cons(x, Empty))).toList
  println(s"With flatMap, each element in $sl is repeated twice: $x57f.")

  println("# Exercise 5.8")
  val x58a = Stream.constant58(1)
  val x58b = x58a.take(5).toList
  println(
    s"The infinite stream of ones is created. The first five is $x58b."
  )

  println("# Exercise 5.9")
  val x59a = Stream.from59(1).take(10).toList
  println(
    s"The infinite stream of increasing integers is created. The first 10 is $x59a."
  )

  println("# Exercise 5.10")
  val x510a = Stream.fibs()
  val x510b = x510a.take(10).toList
  println(s"The first 10 numbers in Fibonacci sequence are $x510b.")

  println("# Exercise 5.11")
  val x511a = Stream.unfold("a")(x => Some((1, x)))
  val x511b = x511a.take(5).toList
  println(
    s"With unfold, an infinite stream of 1 is created. The first five is $x511b."
  )

  println("# Exercise 5.12")
  val x512a = Stream.fibs().take(10).toList
  println(s"The first 10 numbers in Fibonacci sequence are $x512a.")
  val x512b = Stream.from(1).take(10).toList
  println(
    s"The infinite stream of increasing integers is created. The first 10 is $x512b."
  )
  val x512c = Stream.constant(1).take(5).toList
  println(
    s"The infinite stream of 1 is created. The first five is $x512c."
  )

  println("# Exercise 5.13")
  val x513a = ss.map(_ * 10).toList
  println(
    s"With map using unfold, each element in $sl is multipled by 10 to produce $x513a."
  )

  val x513b = ss.take(2).toList
  println(s"The stream's first 2 elements is $x513b.")
  val x513c = ss.take(8).toList
  println(s"The stream's first 8 elements is $x513c.")

  val x513d = ss.takeWhile(_ < 4).toList
  println(s"The starting elements that are less than 4 are $x513d.")

  val ss2 = Stream(6, 7, 8, 9, 10)
  val sl2 = ss2.toList
  val x513e = ss.zipWith(ss2)(_ + _).toList
  println(s"With zipWith, adding each element in $sl and $sl2 gives us $x513e.")

  val ss3 = Stream(6, 7, 8, 9)
  val sl3 = ss3.toList
  val x513f = ss.zipAll(ss3).toList
  println(s"zipAll on $sl and $sl3 gave us $x513f.")

  println("# Exercise 5.14")
  val x514a = Stream(1, 2, 3, 4, 5)
  val x514b = Stream(1, 2, 3)
  val x514c = Stream.startsWith(x514a, x514b)
  val x514d = Stream.startsWith(x514b, x514a)
  val x514e = x514a.toList
  val x514f = x514b.toList
  println(s"Starts with $x514e and $x514f: $x514c and $x514d.")

  val x514g = Stream(1, 2, 3, 4, 5)
  val x514h = Stream(1, 2, 4)
  val x514i = Stream.startsWith(x514g, x514h)
  val x514j = Stream.startsWith(x514h, x514g)
  val x514k = x514g.toList
  val x514l = x514h.toList
  println(s"Starts with $x514k and $x514l: $x514i and $x514j.")

  println("# Exercise 5.15")
  val x515a = Stream(1, 2, 3)
  val x515b = x515a.toList
  val x515c = Stream.tails(x515a)
  val x515d = x515c.toList.map(_.toList)
  println(s"The tails of $x515b are $x515d.")
}
