package tkngch.exercises_fpinscala.chapter14

sealed trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }

}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {

  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell = a
    })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A] {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  /*
   * Ex 14.1 Add a combinator to STArray to fill the array from a Map.
   */
  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST(()): ST[S, Unit]) {
      case ((k, v), x) => x.flatMap(_ => write(k, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] { lazy val value = xs.toArray })

}

object QuickSort {

  /*
   * Ex 14.2 Write the purely functional version of partition and qs.
   */
  // My attempt below does not compile.
  // def partition[S](
  //     arr: STArray[S, Int],
  //     n: Int,
  //     r: Int,
  //     pivot: Int
  // ): ST[S, Int] =
  //   arr
  //     .read(pivot)
  //     .flatMap(pivotVal => {
  //       arr.swap(pivot, r)
  //       STRef(n)
  //         .map(j => {
  //           (n until r).foldLeft(ST(()): ST[S, Unit])((s, i) => {
  //             arr
  //               .read(i)
  //               .flatMap(x => {
  //                 if (x < pivotVal) {
  //                   j.read.flatMap(y => {
  //                     arr.swap(i, y)
  //                     j.write(y + 1)
  //                     ST(()): ST[S, Unit]
  //                   })
  //                 } else {
  //                   ST(()): ST[S, Unit]
  //                 }
  //               })
  //           })
  //           j.read.map(y => { arr.swap(y, r) })
  //           j
  //         })
  //     })

  // The answer uses nested for comprehensions.
  // def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] =
  //   for {
  //     vp <- a.read(pivot)
  //     _ <- a.swap(pivot, r)
  //     j <- STRef(l)
  //     _ <- (l until r).foldLeft(noop[S])((s, i) =>
  //       for {
  //         _ <- s
  //         vi <- a.read(i)
  //         _ <- if (vi < vp) (for {
  //           vj <- j.read
  //           _ <- a.swap(i, vj)
  //           _ <- j.write(vj + 1)
  //         } yield ())
  //         else noop[S]
  //       } yield ()
  //     )
  //     x <- j.read
  //     _ <- a.swap(x, r)
  //   } yield x

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = ???
}
