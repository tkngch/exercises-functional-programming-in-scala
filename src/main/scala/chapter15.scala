package tkngch.exercises_fpinscala.chapter15

import tkngch.exercises_fpinscala.chapter12.Monad

sealed trait Process1[I, O] {
  def apply(s: LazyList[I]): LazyList[O] = this match {
    case Halt1() => LazyList()
    case Await1(recv) =>
      s match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
    case Emit1(h, t) => h #:: t(s)
  }

  def repeat: Process1[I, O] = {
    def go(p: Process1[I, O]): Process1[I, O] = p match {
      case Halt1() => go(this)
      case Await1(recv) =>
        Await1 {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit1(h, t) => Emit1(h, go(t))
    }
    go(this)
  }

  /*
   * Ex 15.5 Implement |> as a method on Process1.
   */
  def |>[O2](p2: Process1[O, O2]): Process1[I, O2] = {
    (this, p2) match {
      case (Halt1(), _)                 => Halt1()
      case (_, Halt1())                 => Halt1()
      case (Await1(recv), Await1(_))    => Await1((s: Option[I]) => recv(s) |> p2)
      case (Emit1(h, t), Await1(recv2)) => t |> recv2(Some(h))
      case (_, Emit1(h2, t2))           => Emit1(h2, this |> t2)
    }
  }

}

object Process1 {

  def liftOne[I, O](f: I => O): Process1[I, O] = Await1 {
    case Some(i) => Emit1(f(i))
    case None    => Halt1()
  }

  def lift[I, O](f: I => O): Process1[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process1[I, I] =
    Await1[I, I] {
      case Some(i) if p(i) => Emit1(i)
      case _               => Halt1()
    }.repeat

  def sum: Process1[Double, Double] = {
    def go(acc: Double): Process1[Double, Double] =
      Await1 {
        case Some(d) => Emit1(d + acc, go(d + acc))
        case None    => Halt1()
      }
    go(0.0)
  }

  /*
   * Ex 15.1 Implement take, drop, takeWhile and dropWhile.
   */
  def take[I](n: Int): Process1[I, I] = {
    Await1[I, I] {
      case Some(i) if n > 0 => Emit1(i, take(n - 1))
      case _                => Halt1()
    }
  }

  def drop[I](n: Int): Process1[I, I] = {
    Await1[I, I] {
      case Some(i) if n > 0  => drop(n - 1)
      case Some(i) if n == 0 => Emit1(i, drop(n))
      case _                 => Halt1()
    }
  }

  def takeWhile[I](f: I => Boolean): Process1[I, I] = {
    Await1[I, I] {
      case Some(i) if f(i) => Emit1(i, takeWhile(f))
      case _               => Halt1()
    }
  }

  def dropWhile[I](f: I => Boolean): Process1[I, I] = {
    Await1[I, I] {
      case Some(i) if f(i)  => dropWhile(f)
      case Some(i) if !f(i) => Emit1(i, dropWhile(x => false))
      case _                => Halt1()
    }
  }

  /*
   * Ex 15.2 Implement count. It should emit the number of elements seen so far.
   */
  def count[I]: Process1[I, Int] = {
    def go(acc: Int): Process1[I, Int] =
      Await1 {
        case Some(i) => Emit1(acc + 1, go(acc + 1))
        case None    => Halt1()
      }
    go(0)
  }

  /*
   * Ex 15.3 Implement mean. It should emit a running average of the value seen so far.
   */
  def mean: Process1[Double, Double] = {
    def go(acc: Double, n: Int): Process1[Double, Double] =
      Await1 {
        case Some(i) => Emit1((acc + i) / (n + 1), go(acc + i, n + 1))
        case None    => Halt1()
      }
    go(0.0, 0)
  }

}

case class Halt1[I, O]() extends Process1[I, O]
case class Await1[I, O](recv: Option[I] => Process1[I, O])
    extends Process1[I, O]
case class Emit1[I, O](head: O, tail: Process1[I, O] = Halt1[I, O]())
    extends Process1[I, O]

trait Process[F[_], O] {
  import Process.{Await, Emit, Halt, End, Kill, Try, await}

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e)          => Try(f(e))
    case Emit(h, t)       => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case End => p
    case err => Halt(err)
  }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
    this match {
      case Halt(err)        => Halt(err)
      case Emit(h, t)       => Try(f(h)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_.flatMap(f)))
    }

  /*
   * Ex 15.10 Define runLog.
   */
  def runLog(implicit m: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
      this match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End)  => m.unit(acc)
        case Halt(err)  => m.fail(err)
        case Await(req, recv) =>
          m.flatMap(m.attempt(req))({ e =>
            go(Try(recv(e)), acc)
          })
      }
    go(this, IndexedSeq())
  }

  def onComplete(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e)    => Halt(e)
    case Await(req, recv) =>
      await(req) {
        case Left(Kill) => this.asFinalizer
        case x          => recv(x)
      }
  }
}

object Process {
  case class Await[F[_], A, O](
      req: F[A],
      recv: Either[Throwable, A] => Process[F, O]
  ) extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception
  case object Kill extends Exception

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def await[F[_], A, O](req: F[A])(
      recv: Either[Throwable, A] => Process[F, O]
  ): Process[F, O] = Await(req, recv)

  /*
   * Ex 15.11 Implement eval and eval_.
   */
  def eval[F[_], A](a: F[A]): Process[F, A] =
    await(a)({
      case Left(e)  => Halt(e)
      case Right(x) => Emit(x, Halt(End))
    })

  def eval_[F[_], A, B](a: F[A]): Process[F, B] =
    await(a)({
      case Left(e)  => Halt(e)
      case Right(x) => Halt(End)
    })

  case class T[I, I2]() {
    sealed trait f[X] { def get: Either[I => X, I2 => X] }
  }
  type Tee[I, I2, O] = Process[T[I, I2]#f, O]
  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] = ???

  /*
   * Ex 15.12 Implement join using existing primitives.
   */
  def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] =
    p.flatMap(a => a)

}

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable, A]]
  def fail[A](t: Throwable): F[A]
}

object Main extends App {
  println("Exercise 15.1")
  val s = LazyList(0, 1, 2, 3, 4, 5, 6)
  val taken = Process1.take(3)(s).toList
  println(s"The first 2 elements in ${s.toList} is $taken.")

  println("Exercise 15.2")
  println(
    s"The count for ${s.toList} is ${Process1.count(s).toList}."
  )

  println("Exercise 15.3")
  val ss = LazyList(0, 1, 2, 3, 4, 5, 6.0)
  println(
    s"The running mean for ${ss.toList} is ${Process1.mean(ss).toList}."
  )
}
