package tkngch.exercises_fpinscala.chapter02

object Ex2dot2 {
  /*
   * Implement `isSorted`, which checks whether an `Array[A]` is sorted according to a
   * given comparison function.
   *
   * Note that this exercise asks for Array[A], the implementation below uses List[A].
   * Because I do not want to work with a mutable object (i.e., Array) when an immutable
   * alternative (i.e., List) is readily available.
   */
  def isSorted[A](array: List[A])(func: (A, A) => Boolean): Boolean = {
    array match {
      case first :: second :: rest =>
        if (func(first, second)) isSorted(second :: rest)(func) else false
      case first :: Nil => true
      case _            => true
    }
  }
}

object Ex2dot3 {
  /*
   * Let's look at another example, currying, which converts a function `f` of two
   * arguments into a function of one argument that partially applies `f`. Here again
   * there's only one implementation that compiles. Write this implementation.
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) =>
      { (b: B) =>
        f(a, b)
      }
  }
}

object Ex2dot5 {
  /*
   * Implement the higher-order function that composes two functions.
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}

object Main extends App {
  val sorted = Ex2dot2.isSorted(List(1, 2, 3))(_ < _)
  println(sorted)
  val unsorted = Ex2dot2.isSorted(List(3, 2, 1))(_ < _)
  println(unsorted)
}
