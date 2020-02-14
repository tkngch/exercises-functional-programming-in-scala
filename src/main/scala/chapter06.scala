package tkngch.exercises_fpinscala.chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /*
   * Ex 6.1 Write a function that uses RNG.nextInt to generate a random integer between
   * 0 and Int.MaxValue (inclusive). Make sure to handle the corner case when nextInt
   * returns Int.MinValue, which doesn't have a non-negative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rng2) = rng.nextInt
    if (0 <= x) (x, rng2) else nonNegativeInt(rng2)
  }

  /*
   * Ex 6.2 Write a function to generate a Double between 0 and 1, not including 1.
   */
  // Note: this method, double, is reimplemented later.
  def double_(rng: RNG): (Double, RNG) = {
    val (x, rng2) = nonNegativeInt(rng)
    (x.toDouble / Int.MaxValue.toDouble, rng2)
  }

  /*
   * Ex 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and
   * a (Double, Double, Double) 3-tuple.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (xint, rng2) = nonNegativeInt(rng)
    val (xdou, rng3) = double(rng2)
    ((xint, xdou), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (xdou, rng2) = double(rng)
    val (xint, rng3) = nonNegativeInt(rng2)
    ((xdou, xint), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /*
   * Ex 6.4 Write a function to generate a list of random integers.
   */
  // This method, ints, is reimplemented later
  def ints_(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List.empty, rng)
    else {
      val (x, rng2) = nonNegativeInt(rng)
      val (y, rng3) = ints(count - 1)(rng2)
      (x :: y, rng3)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // map is reimplement later.
  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (x, rng2) = s(rng)
      (f(x), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(x => x - x % 2)

  /*
   * Ex 6.5 Use map to reimplement double.
   */
  def double: Rand[Double] =
    map(int)(x => x.toDouble / Int.MaxValue)

  def boolean: Rand[Boolean] =
    map(int)(x => x % 2 == 0)

  /*
   * Ex 6.6 Write the implementation of map2 based on the following signature. This
   * function takes two actions, ra and rb, and a function f for combining their
   * results, and returns a new action that combines them.
   */
  // map2 is reimplement later.
  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /*
   * Ex 6.7 Implement `sequence` for combining a List of transitions into a single
   * transition.  Use it to reimplement the ints function you wrote before.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case head :: tail => {
          val (y, rng2) = sequence(tail)(rng)
          val (x, rng3) = head(rng2)
          ((x :: y), rng3)
        }
        case Nil => (List.empty, rng)
      }
    }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /*
   * Ex 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, rng2) = f(rng)
      g(x)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(x => {
      rng => {
        val mod = x % n
        if (x + (n - 1) - mod >= 0) (mod, rng)
        else nonNegativeLessThan(n)(rng)
      }
    })

  /*
   * Ex 6.9 Reimplement map and map2 in terms of flatMap.
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => { rng => (f(a), rng) })

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => { rng => (f(a, b), rng) }))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class State[S, +A](run: S => (A, S)) {
  /*
   * Ex 6.10 Generalise the functions unit, map, map2, flatMap, and sequence.
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(rng => {
      val (x, rng2) = this.run(rng)
      f(x).run(rng2)
    })

  def map[B](f: A => B): State[S, B] =
    this.flatMap(a => State(rng => (f(a), rng)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(a => rb.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    State(rng =>
      fs match {
        case head :: tail => {
          val (y, rng2) = sequence(tail).run(rng)
          val (x, rng3) = head.run(rng2)
          ((x :: y), rng3)
        }
        case Nil => (List.empty, rng)
      }
    )

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Main extends App {
  val rng = RNG.SimpleRNG(417)

  println("# Exercise 6.1")
  val (x61a, _) = RNG.nonNegativeInt(rng)
  println(s"A generated random number is $x61a.")

  println("# Exercise 6.2")
  val (x62a, _) = RNG.double(rng)
  println(s"A generated random double is $x62a.")

  println("# Exercise 6.3")
  val (x63a, rng63a) = RNG.intDouble(rng)
  println(s"intDouble is $x63a.")
  val (x63b, rng63b) = RNG.doubleInt(rng63a)
  println(s"doubleInt is $x63b.")
  val (x63c, rng63c) = RNG.double3(rng63b)
  println(s"double3 is $x63c.")

  println("# Exercise 6.4")
  val (x64a, rng64a) = RNG.ints(3)(rng)
  println(s"The randomly generated 3 integers are $x64a.")

  println("# Exercise 6.5")
  val (x65a, rng65a) = RNG.int(rng)
  println(s"A randomly generated integer is $x65a.")
  val (x65b, rng65b) = RNG.double(rng65a)
  println(s"A randomly generated double is $x65b.")

  println("# Exercise 6.6")
  val (x66a, rng66a) = RNG.map2(RNG.nonNegativeInt, RNG.double)((_, _))(rng)
  println(s"intDouble is $x66a.")

  println("# Exercise 6.7")
  val (x67a, rng67a) =
    RNG.sequence(
      List(RNG.nonNegativeInt(_), RNG.nonNegativeInt(_), RNG.double(_))
    )(rng)
  println(s"intIntDouble is $x67a.")
  val n67 = 4
  val (x67b, rng67b) =
    RNG.sequence(List.fill(n67)(RNG.nonNegativeInt(_)))(rng67a)
  println(s"The list of $n67 random integers is $x67b.")

  println("# Exercise 6.8")
  val (x68a, rng68a) = RNG.nonNegativeLessThan(10)(rng)
  println(s"A random integer less than 10 is $x68a.")

  println("# Exercise 6.9")
  val (x69a, rng69a) =
    RNG.map(RNG.nonNegativeInt)(x => x.toDouble / Int.MaxValue.toDouble)(rng)
  println(s"A random double is $x69a.")
  val (x69b, rng69b) =
    RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)((_, _))(rng69a)
  println(s"Two random integers are $x69b.")

  println("# Exercise 6.10")
  val state = State(RNG.nonNegativeInt)
  val (x610a, rng610a) = state.map(x => x).run(rng)
  println(s"A randomly generated integer is $x610a.")

  val states = List.fill(3)(State(RNG.nonNegativeInt))
  val (x610b, rng610b) = State.sequence(states).run(rng610a)
  println(s"A randomly generated sequence of integers is $x610b.")
}
