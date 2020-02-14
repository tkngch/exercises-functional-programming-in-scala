package tkngch.exercises_fpinscala.chapter04

object Ex4dot1 {
  /*
   * Implement all of the preceding functions on Option.
   */
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case Some(x) => Some(f(x))
        case None    => None
      }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this.map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B =
      this match {
        case Some(x) => x
        case None    => default
      }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this.map(x => Some(x)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      this.flatMap(x => if (f(x)) Some(x) else None)
  }

  def main(): Unit = {
    println("# Exercise 4.1")
    val s = Some(1)
    val m = s.map(_ * 100)
    println(s"After mapping $s with `_ * 100`, we get $m.")
    val f = s.flatMap(x => Some(x * 100))
    println(s"After flat-mapping $s with `Some(_ * 100)`, we get $f.")
    val g = s.getOrElse(-99)
    println(s"The value of $s is $g.")
    val o = s.orElse(None)
    println(s"$s is $o.")
    val fi = s.filter(x => x % 2 == 0)
    println(s"After filtering to preserve the even numbers, $s turns $fi.")

    val n: Option[Int] = None
    val mn = n.map(_ * 100)
    println(s"After mapping $n with `_ * 100`, we get $mn.")
    val fn = n.flatMap(x => Some(x * 100))
    println(s"After flat-mapping $n with `Some(_ * 100)`, we get $fn.")
    val gn = n.getOrElse(-999)
    println(s"The value of $n is $gn.")
    val on = n.orElse(Some(-999))
    println(s"$n is $on.")
    val fin = n.filter(x => x % 2 == 0)
    println(s"After filtering to preserve the even numbers, $n turns $fin.")
  }
}

object Ex4dot3 {
  /*
   * Write a generic function `map2` that combines two Option values using a binary
   * function. If either Option value is None, then the return value is too.
   */
  import Ex4dot1.{Option, Some, None}

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a match {
      case Some(x) =>
        b match {
          case Some(y) => Some(f(x, y))
          case None    => None
        }
      case None => None
    }

  def map2InBook[A, B, C](a: Option[A], b: Option[B])(
      f: (A, B) => C
  ): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  def main(): Unit = {
    println("# Exercise 4.3")
    val a = Some(1)
    val b = Some("a")
    val c = map2(a, b)((x, y) => (x, y))
    println(s"Using map2, $a and $b are placed in a tuple, $c.")
    val d = map2InBook(a, b)((x, y) => (x, y))
    println(s"Using map2, $a and $b are placed in a tuple, $d.")
  }
}

object Ex4dot4 {
  /*
   * Write a function `sequence` that combines a list of Options into one Option
   * containing a list of all the Some values in the original list. If the original list
   * contains None even once, the result of the function should be None.
   */
  import Ex4dot1.{Option, Some, None}

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case head :: tail =>
        head.flatMap(x => sequence(tail).map(y => (x :: y)))
      case Nil => Some(List.empty)
    }

  def main(): Unit = {
    println("# Exercise 4.4")
    val a = List(Some(1), Some(2), Some(3))
    val s = sequence(a)
    println(s"$a is sequenced into $s.")

    val b = List(Some(1), Some(2), None, Some(3))
    val n = sequence(b)
    println(s"$b is sequenced into $n.")
  }
}

object Ex4dot5 {
  /* Implement `traverse` function and implement `sequence` in terms of `traverse`.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case head :: tail =>
        f(head).flatMap(y => traverse(tail)(f).map(z => (y :: z)))
      case Nil => Some(List.empty)
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def main(): Unit = {
    println("# Exercise 4.5")
    val a = (1 to 5).toList
    val f = (x: Int) => Some(x * 2)
    val y = traverse(a)(f)
    println(s"Application of traverse on $a to multiply by 2 resulted in $y.")

    val as = List(Some(1), Some(2), Some(3))
    val s = sequence(as)
    println(s"$as is sequenced into $s.")

    val b = List(Some(1), Some(2), None, Some(3))
    val n = sequence(b)
    println(s"$b is sequenced into $n.")
  }
}

object Ex4dot6 {
  /* Implement versions of map, flatMap, orElse and map2 on Either that operate on the
   * Right value.
   */
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Left(x)  => Left(x)
        case Right(x) => Right(f(x))
      }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(x)  => Left(x)
        case Right(x) => f(x)
      }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(x)  => b
        case Right(x) => Right(x)
      }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(x => b.map(y => f(x, y)))
  }

  def main(): Unit = {
    println("# Exercise 4.6")
    val s = Right(1)
    val m = s.map(_ * 100)
    println(s"After mapping $s with `_ * 100`, we get $m.")
    val f = s.flatMap(x => Right(x * 100))
    println(s"After flat-mapping $s with `Right(_ * 100)`, we get $f.")
    val o = s.orElse(Left("Error"))
    println(s"$s or else $o.")

    val n = Left("left")
    val mn = n.map(x => 1)
    println(s"After mapping $n, we get $mn.")
    val fn = n.flatMap(x => Right(x))
    println(s"After flat-mapping $n with `Right(_)`, we get $fn.")
    val on = n.orElse(Left("left2"))
    println(s"$n or else $on.")
  }
}

object Ex4dot7 {
  /*
   * Implement sequence and traverse for Either. These should return the first error
   * that's encountered, if there is one.
   */
  import Ex4dot6.{Either, Left, Right}
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case head :: tail =>
        f(head).flatMap(y => traverse(tail)(f).map(z => (y :: z)))
      case Nil => Right(List.empty)
    }

  def main(): Unit = {
    println("# Exercise 4.7")
    val a = (1 to 5).toList
    val f = (x: Int) => Right(x * 2)
    val y = traverse(a)(f)
    println(s"Application of traverse on $a to multiply by 2 resulted in $y.")

    val as = List(Right(1), Right(2), Right(3))
    val s = sequence(as)
    println(s"$as is sequenced into $s.")

    val b = List(
      Right(1),
      Left("The first error."),
      Right(2),
      Left("The second error.")
    )
    val n = sequence(b)
    println(s"$b is sequenced into $n.")
  }
}

object Main extends App {
  Ex4dot1.main
  Ex4dot3.main
  Ex4dot4.main
  Ex4dot5.main
  Ex4dot6.main
  Ex4dot7.main
}
