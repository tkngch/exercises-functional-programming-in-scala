package tkngch.exercises_fpinscala.chapter09

import scala.language.implicitConversions
import scala.util.matching.Regex

import tkngch.exercises_fpinscala.chapter08.{Gen, Prop}

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  /** Recognise and return a single String
    */
  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  /** Return a portion of the input examined by the parser if successful.
    */
  def slice[A](p: Parser[A]): Parser[String]

  /** Always succeed with the input value.
    */
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /** Apply the function to the result of parser, if successful.
    */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(x => succeed(f(x)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /** Sequence two parsers, and run one parser, followed by another, assuming the first
    *  is successful. Return the pair (tuple) of their results if both succeed.
    */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(x => p2.map(y => (x, y)))

  /** Choose between two parsers, first attemping p1 and then p2 if p1 fails.
    */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /** Parse a single character input against `c`.
    */
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def letter: Parser[String] = regex("[a-zA-Z]".r)
  def digit: Parser[String] = regex("[0-9]".r)
  def whitespace: Parser[String] = regex("\\w".r)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  /** Recognise zero or more repetitions of the character and return the number of
    *  characters it has seen.
    */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  /** Recognise one or more repetition of the character.
    */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    // product(p, p2).map(f.tupled)
    p.flatMap(x => p2.map(y => f(x, y)))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many(): Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice(): Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productLaw[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(
        in: Gen[String]
    ): Prop =
      equal(
        p1.product(p2).product(p3).map(x => (x._1._1, x._1._2, x._2)),
        p3.product(p2).product(p1).map(x => (x._2, x._1._2, x._1._1))
      )(in)
  }

}

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = ???
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))
  def advanceBy(n: Int) = copy(offset = offset + n)
}

case class ParseError(stack: List[(Location, String)])

object Main extends App {
  println("# Chapter 9")
}
