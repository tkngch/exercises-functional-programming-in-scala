package tkngch.exercises_fpinscala.chapter08

import language.implicitConversions
import java.util.concurrent.Executors

import tkngch.exercises_fpinscala.chapter05.Stream
import tkngch.exercises_fpinscala.chapter06.{RNG, State}
import tkngch.exercises_fpinscala.chapter07.Par.Par

/*
 * Ex 8.3 Implement && as a method for the following representation of Prop.
 */
trait Prop83 {
  self =>
  def check: Boolean
  def &&(p: Prop83): Prop83 = new Prop83 {
    def check: Boolean = self.check && p.check
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  /*
   * Ex 8.6 Implement flatMap, and then use it to implement the more dynamic version of
   * listOfN. Put flatMap and listOfN in the Gen class.
   */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(this.sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(x => Gen(State.sequence(List.fill(x)(this.sample))))

  /*
   * Ex 8.10 Implement helper functions for converting Gen to SGen. Add this to a method
   * on Gen.
   */
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))

  def map[B](f: A => B): Gen[B] =
    Gen(this.sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(this.sample.map2(g.sample)(f))
}

object Gen {

  /*
   * Ex 8.4 Implement Gen.choose using the following representation of Gen. It should
   * generate integers in the range start to stopExclusive.
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state = State(RNG.nonNegativeLessThan(stopExclusive - start))
    Gen(state.map(_ + start))
  }

  /*
   * Ex 8.5 Implement unit, boolean, and listOfN for Gen.
   */
  def unit[A](a: => A): Gen[A] = Gen(State(x => (a, x)))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /*
   * Ex 8.7 Implement union, for combining two generators of the same type into one, by
   * pulling values from each generator with equal likelihood.
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    val g = Gen(State(RNG.nonNegativeLessThan(10)))
    g.flatMap(x => if (x % 2 == 0) g1 else g2)
  }

  /*
   * Ex 8.10 Implement a listOf combinator that doesn't accept an explicit size.
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(x => Gen(State.sequence(List.fill(x)(g.sample))))

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  /*
   * Ex 8.13 Define listOf1 for generating nonempty list, and then update your
   * specification of max to use this generator.
   */
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(x => Gen(State.sequence(List.fill(x.max(1))(g.sample))))

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(
      State(RNG.double)
        .flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample)
    )
  }
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
}

import Prop.{TestCases, Result, Passed, Falsified, MaxSize, Proved}
// import Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  /*
   * Ex 8.9 Implement && and || for composing Prop values.
   */
  def &&(p: Prop): Prop = Prop { (size, cases, rng) =>
    this.run(size, cases, rng) match {
      case Passed          => p.run(size, cases, rng)
      case Proved          => p.run(size, cases, rng)
      case Falsified(f, s) => Falsified(f, s)
    }
  }

  def ||(p: Prop): Prop = Prop { (size, cases, rng) =>
    this.run(size, cases, rng) match {
      case Passed          => Passed
      case Proved          => Proved
      case Falsified(_, _) => p.run(size, cases, rng)
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, success: SuccessCount)
      extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop((_, n, rng) => f(n, rng))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop((n, rng) =>
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map({
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        })
        .find(_.isFalsified)
        .getOrElse(Passed)
    )

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop((max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max

      val props: Stream[Prop] =
        Stream.from(0).take(n.min(max) + 1).map(x => forAll(g(x))(f))

      val prop: Prop = props
        .map(p => Prop((max, n, rng) => p.run(max, casesPerSize, rng)))
        .toList
        .reduce(_ && _)
      prop.run(max, n, rng)
    })

  def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  ) // `a -> b` is syntax sugar for `(a,b)`

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }
}

object Examples {
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(Gen.listOf1(smallInt))(ns => {
    val maxVal = ns.max
    !ns.exists(_ > maxVal)
  })

  val sortProp = Prop.forAll(Gen.listOf(smallInt))(ns => {
    val sorted = ns.sorted

    // same length
    (ns.size == sorted.size) && (// same items
    sorted
      .map(ns.contains(_))
      .forall(_ == true)) && (
      // earlier is smaller than or equal to later
      sorted
        .zip(sorted.tail)
        .map({ case (a, b) => a <= b })
        .forall(_ == true)
      )
  })

}

object Main extends App {
  val rng = RNG.SimpleRNG(417)

  println("# Exercise 8.4")
  val start = 10
  val stopExclusive = 20
  val (x84a, rng84a) = Gen.choose(start, stopExclusive).sample.run(rng)
  println(s"A random number between $start and $stopExclusive is $x84a.")

  println("# Exercise 8.5")
  val (x85a, rng85a) = Gen.unit(1).sample.run(rng)
  println(s"The unit of 1 is $x85a.")
  val (x85b, rng85b) = Gen.boolean.sample.run(rng85a)
  println(s"A random boolean is $x85b.")
  val (x85c, rng85c) = Gen.listOfN(4, Gen.boolean).sample.run(rng85b)
  println(s"A list of random boolean is $x85c.")

  println("# Exercise 8.6")
  val n86 = Gen(State(RNG.nonNegativeLessThan(10)))
  val gen86 = Gen(State(RNG.nonNegativeLessThan(20)))
  val (x86a, rng86a) = gen86.listOfN(n86).sample.run(rng)
  println(s"A list of random integers is $x86a.")

  println("# Exercise 8.7")
  val g87a = Gen(State(RNG.nonNegativeLessThan(10)))
  val g87b = Gen(State(RNG.nonNegativeLessThan(100)))
  val (x87a, _) = Gen.union(g87a, g87b).sample.run(rng)
  println(s"A random number is $x87a.")
}
