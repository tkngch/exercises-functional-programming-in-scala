package tkngch.exercises_fpinscala.chapter07

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def get[A](a: Par[A]): A = ???

  /*
   * Ex 7.1 What is `Par.map2`'s signature?
   */
  def map2_[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def unit[A](a: => A): Par[A] = (s: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (s: ExecutorService) => {
      val af = a(s)
      val bf = b(s)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    (s: ExecutorService) => s.submit(new Callable[A] { def call = a(s).get })

  /*
   * Ex 7.4 Using lazyUnit, write a function to convert any function A => B to one that
   * evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] = { x =>
    lazyUnit(f(x))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  /*
   * Ex 7.5 Write this function, called sequence.
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case head :: tail => map2(head, sequence(tail))(_ :: _)
      case Nil          => unit(List.empty)
    }

  def sequence2[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty: List[A]))((x, y) => map2(x, y)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /*
   * Ex 7.6 Implement parFilter, which filters elements of a list in parallel.
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // List[Par[Option[A]]
    val lpo = as.map(asyncF(x => if (f(x)) Some(x) else None))
    // Par[List[Option[A]]
    val plo = sequence(lpo)
    map(plo)(x =>
      x.foldRight(List.empty: List[A])((a, b) =>
        if (a.nonEmpty) a.get :: b else b
      )
    )
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](f: => Par[A]): Par[A] =
    s => f(s)

  /*
   * Ex 7.11 Implement choiceN and then choice in terms of choiceN.
   */
  def choiceN_[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    (es: ExecutorService) => {
      val index = run(es)(n).get
      run(es)(choices.apply(index))
    }

  def choice_[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(es => {
      if (run(es)(cond).get) run(es)(unit(1)) else run(es)(unit(0))
    })(
      List(f, t)
    )

  /*
   * Ex 7.12 Implement choiceMap.
   */
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    (es: ExecutorService) => {
      val k = run(es)(key).get
      run(es)(choices.apply(k))
    }

  /*
   * Ex 7.13 Implement the new primitive chooser and then use it to implement choice and
   * choiceN.
   */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    (es: ExecutorService) => {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(x => if (x) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(x => choices.apply(x))

  /*
   * Ex 7.14 Implement join. Can you implement flapMap using join, and join using
   * flatMap?
   */
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(unit((es: ExecutorService) => run(es)(f(run(es)(a).get))))

  def joinF[A](a: Par[Par[A]]): Par[A] =
    chooser(a)(x => x)

}

object Main extends App {}
