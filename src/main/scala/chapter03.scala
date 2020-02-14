package tkngch.exercises_fpinscala.chapter03

object Ex3dot2 {
  /*
   * Implement the function `tail` for removing the first element of a `List`. Note that
   * the function takes constant time. What are different choices you could make in your
   * implementation if the `List` is `Nil`?
   */
  def tail[A](list: List[A]): List[A] =
    list match {
      case head :: tail => tail
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.2")
    val as1 = (1 to 3).toList
    val t1 = tail(as1)
    println(s"The tail of $as1 is $t1.")

    val as2 = List(1)
    val t2 = tail(as2)
    println(s"The tail of $as2 is $t2.")

    val as3 = List.empty
    val t3 = tail(as3)
    println(s"The tail of $as3 is $t3.")
  }
}

object Ex3dot3 {
  /*
   * Using the same idea, implement the function `setHead` for replacing the first
   * element of a `List` with a different value.
   */
  def setHead[A](list: List[A], newHead: A): List[A] =
    list match {
      case head :: tail => newHead :: tail
      case Nil          => List(newHead)
    }

  def main(): Unit = {
    println("# Exercise 3.3")
    val as1 = (1 to 3).toList
    val newHead = 10
    val as2 = setHead(as1, newHead)
    println(s"After replacing the head of $as1 with $newHead, we get $as2.")
  }
}

object Ex3dot4 {
  /*
   * Generalise `tail` to the function `drop`, which removes the first n elements from a
   * list.
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else
      l match {
        case head :: tail => drop(tail, n - 1)
        case Nil          => Nil
      }

  def main(): Unit = {
    println("# Exercise 3.4")
    val as1 = (1 to 5).toList
    val n1 = 0
    val d1 = drop(as1, n1)
    println(s"After dropping $n1 elements from $as1, we obtain $d1.")

    val as2 = (1 to 5).toList
    val n2 = 2
    val d2 = drop(as2, n2)
    println(s"After dropping $n2 elements from $as2, we obtain $d2.")
  }
}

object Ex3dot5 {
  /*
   * Implement `dropWhile`, which removes elements from the `List` prefix as long as
   * they match a predicate.
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case head :: tail => if (f(head)) dropWhile(tail)(f) else head :: tail
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.5")
    val as = List(10, 20, 7, 2)
    val d = dropWhile(as)(a => a % 2 == 0)
    println(s"After dropping the first even numbers from $as, we obtain $d.")
  }
}

object Ex3dot6 {
  /* Implement a function, `init`, that returns a `List` consisting of all but the last
   * element of a `List`.
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case head :: Nil  => List(head)
      case head :: tail => init(tail)
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.6")
    val a = (1 to 6).toList
    val last = init(a)
    println(s"A list with the last element of $a is $last.")
  }

}

object Ops {
  def foldRight[A, B](as: List[A], init: B)(f: (A, B) => B): B =
    as match {
      case Nil          => init
      case head :: tail => f(head, foldRight(tail, init)(f))
    }
}

object Ex3dot7 {
  /* Can `product`, implemented using `foldRight`, immediately halt the recursion and
   * return `0.0` if it encounters a `0.0`?
   */
  import Ops.foldRight

  def product(ds: List[Double]): Double =
    ds match {
      case head :: tail => if (head == 0.0) 0.0 else head * product(tail)
      case Nil          => ???
    }

  // With foldRight, I do not see how to short circuit.
  def productF(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def main(): Unit = {
    println("# Exercise 3.7")
    val a = List(1, 2, 3, 0.0, 4.0)
    val p = product(a)
    println(s"The product of $a is $p.")
  }
}

object Ex3dot8 {
  /*
   * See what happens when you pass `Nil` to `foldRight`.
   */
  import Ops.foldRight

  def main(): Unit = {
    println("# Exercise 3.8")
    val a = (1 to 3).toList
    val x = foldRight(a, Nil: List[Int])(_ :: _)
    println(s"When you pass $a and Nil to foldRight, you get $x.")
  }
}

object Ex3dot9 {
  /*
   * Compute the length of a list using `foldRight`.
   */
  import Ops.foldRight

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => y + 1)

  def main(): Unit = {
    println("# Exercise 3.9")
    val a = List("a", "b", "c", "d", "e")
    val len = length(a)
    println(s"The length of $a is $len.")
  }
}

object Ex3dot10 {
  /*
   * The implementation of `foldRight` is not tail-recursive and will result in a
   * `StackOverflowError` for large lists. Write another generate list-recursion
   * function, `foldLeft`, that is tail-recursive.
   */
  import Ops.foldRight

  val maxNum = 100000
  val a = (1 to maxNum).toList
  val sum = (1 + maxNum) * maxNum / 2

  def test[A, B](f: (List[Int], Int) => ((Int, Int) => Int) => Int): Unit = {
    val ei =
      try {
        Right(f(a, 0)(_ + _))
      } catch {
        case e: Throwable =>
          Left(e.toString)
      }
    if (ei.isLeft) {
      val res = ei.swap.getOrElse("error")
      println(s"-> Resulted in $res.")
    } else {
      val res = ei.getOrElse(0)
      println(s"-> The result is $res, where the correct answer is $sum.")

    }
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], init: B)(f: (B, A) => B): B =
    as match {
      case head :: tail => foldLeft(tail, f(init, head))(f)
      case Nil          => init
    }

  def main(): Unit = {
    println("# Exercise 3.10")
    println("Testing foldRight")
    test(foldRight)
    println("Testing foldLeft")
    test(foldLeft)
  }
}

object Ex3dot11 {
  /* Write `sum`, `product`, and a function to compute the length of a list using
   * `foldLeft`.
   */
  import Ex3dot10.foldLeft

  def sum(as: List[Double]): Double = foldLeft(as, 0.0)(_ + _)
  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => 1 + x)

  def main(): Unit = {
    println("# Exercise 3.11")
    val a = List(1, 2, 3, 4, 5, 6.0)
    val s = sum(a)
    val p = product(a)
    val l = length(a)
    println(s"For $a, the sum is $s, the product is $p, and the length is $l.")
  }
}

object Ex3dot12 {
  /*
   * Write a function that returns the reverse of a list.
   */
  import Ex3dot10.foldLeft
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List.empty: List[A])((x, y) => y :: x)

  def main(): Unit = {
    println("# Exercise 3.12")
    val a = (1 to 4).toList
    val r = reverse(a)
    println(s"The reverse of $a is $r.")
  }
}

object Ex3dot14 {
  /* Implement `append` in terms of either `foldLeft` or `foldRight`
   */
  import Ops.foldRight
  import Ex3dot10.foldLeft

  def append[A](as: List[A], v: A): List[A] =
    foldRight(as, List(v))((x, y) => x :: y)

  def prepend[A](as: List[A], v: A): List[A] =
    // with foldLeft, an element is added at the beginning of List
    foldLeft(as, List(v))((x, y) => x ::: List(y))

  def main(): Unit = {
    println("# Exercise 3.14")
    val a = (1 to 3).toList
    val x = 100
    val p = append(a, x)
    println(s"When $x is appended to $a, we get $p.")
    val r = prepend(a, x)
    println(s"When $x is prepended to $a, we get $r.")
  }
}

object Ex3dot15 {
  /*
   * Write a function that concatenates a list of lists into a single list.
   */
  import Ex3dot10.foldLeft
  import Ex3dot14.append

  def concatenate[A](x: List[List[A]]): List[A] =
    // If I use :::, I feel I am cheating...
    // foldLeft(x, List.empty: List[A])((x, y) => x ::: y)
    foldLeft(x, List.empty: List[A])((a: List[A], b: List[A]) =>
      foldLeft(b, a)((c: List[A], d: A) => append(c, d))
    )

  def main(): Unit = {
    println("# Exercise 3.15")
    val as = List((1 to 2).toList, (10 to 12).toList, (20 to 22).toList)
    val c = concatenate(as)
    println(s"When we concatenate $as, we get $c.")
  }
}

object Ex3dot16 {
  /* Write a function that transforms a list of integers by adding 1 to each element.
   */
  def addOne(a: List[Int]): List[Int] =
    a match {
      case head :: tail => head + 1 :: addOne(tail)
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.16")
    val a = (1 to 3).toList
    val y = addOne(a)
    println(s"After adding one to each element in $a, we get $y.")
  }
}

object Ex3dot17 {
  /*
   * Write a function tha turns each value in a List[Double] into a String.
   */
  def doubleToString(a: List[Double]): List[String] =
    a match {
      case head :: tail => head.toString :: doubleToString(tail)
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.17")
    val a = List(1.0, 2.5, 3.9)
    val s = doubleToString(a)
    println(s"After turning each element of $a into a String, we get $s.")
  }

}

object Ex3dot18 {
  /*
   * Write a function `map` that generalises modifying each element in a list while
   * maintaining the structure of the list.
   */
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case head :: tail => f(head) :: map(tail)(f)
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.18")
    val a = List(1.1, 2.2, 3.3, 4.4, 5.5, 6.6)
    val y = map(a)(_.round)
    println(s"When we round each element in $a, we get $y.")
  }
}

object Ex3dot19 {
  /*
   * Write a function filter that removes elements from a list unless they satisfy a
   * given predicate.
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case head :: tail =>
        if (f(head)) head :: filter(tail)(f) else filter(tail)(f)
      case Nil => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.19")
    val a = (1 to 10).toList
    val e = filter((1 to 10).toList)(x => x % 2 == 0)
    println(s"The even numbers in $a is $e.")
  }
}

object Ex3dot20 {
  /*
   * Write a function flatMap that works like map except that the function given will
   * return a list instead of a single result, and that list should be inserted into the
   * final resulting list.
   */
  import Ex3dot15.concatenate

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case head :: tail => concatenate(List(f(head), flatMap(tail)(f)))
      case Nil          => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.20")
    val a = (1 to 5).toList
    val r = flatMap(a)(x => List(x, x))
    println(s"flatMap with $a to repeat each element twice resulted in $r.")
  }
}

object Ex3dot21 {
  /*
   * Use flatMap to implement filter.
   */
  import Ex3dot20.flatMap

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else List.empty)

  def main(): Unit = {
    println("# Exercise 3.21")
    val a = (1 to 10).toList
    val e = filter((1 to 10).toList)(x => x % 2 == 0)
    println(s"After removing the odd numbers from $a with flatMap, we get $e.")
  }
}

object Ex3dot22 {
  /*
   * Write a function that accepts two lists and constructs a new list by adding
   * corresponding elements.
   */
  def addElements(a1: List[Int], a2: List[Int]): List[Int] =
    a1 match {
      case h1 :: t1 =>
        a2 match {
          case h2 :: t2 => h1 + h2 :: addElements(t1, t2)
          case Nil      => Nil
        }
      case Nil => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.22")
    val a = (1 to 3).toList
    val b = (4 to 6).toList
    val x = addElements(a, b)
    println(s"Adding each element in $a and $b gives us $x.")
    val c = (4 to 7).toList
    val y = addElements(a, c)
    println(s"Adding each element in $a and $c gives us $y.")
  }
}

object Ex3dot23 {
  /*
   * Generalise the function you just wrote so that it's not specific to integers or
   * addition.
   */
  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    a1 match {
      case h1 :: t1 =>
        a2 match {
          case h2 :: t2 => f(h1, h2) :: zipWith(t1, t2)(f)
          case Nil      => Nil
        }
      case Nil => Nil
    }

  def main(): Unit = {
    println("# Exercise 3.23")
    val a = (1 to 3).toList
    val b = (4 to 6).toList
    val x = zipWith(a, b)(_ + _)
    println(s"With zipWith, adding each element in $a and $b gives us $x.")
  }
}

object Ex3dot24 {
  /*
   * Implement `hasSubsequence` for checking whether a List constains another List as a
   * subsequence
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case p1 :: p2 :: p3 =>
        sub match {
          case b1 :: b2 :: b3 =>
            if ((p1 == b1) && (p2 == b2)) hasSubsequence(p2 :: p3, b2 :: b3)
            else if ((p1 == b1) && (p2 != b2)) false
            else hasSubsequence(p2 :: p3, sub)
          case b1 :: Nil =>
            if (p1 == b1) true else hasSubsequence(p2 :: p3, b1 :: Nil)
          case b1 :: b2 => ((p1 == b1) && (p2 == b2))
          case Nil      => false
        }
      case p1 :: p2 =>
        sub match {
          case b1 :: b2 :: b3 => false
          case b1 :: Nil      => (p1 == b1)
          case b1 :: b2       => ((p1 == b1) && (p2 == b2))
          case Nil            => false
        }
      case Nil => true
    }

  def main(): Unit = {
    println("# Exercise 3.24")
    val a = (1 to 4).toList
    val b = List(2, 3)
    val c = List(4)
    val d = List(1, 2, 4)
    val e = List(2)
    val f = List(5)
    println(s"Does $a has $b as a subsequence? " + hasSubsequence(a, b))
    println(s"Does $a has $c as a subsequence? " + hasSubsequence(a, c))
    println(s"Does $a has $d as a subsequence? " + hasSubsequence(a, d))
    println(s"Does $a has $e as a subsequence? " + hasSubsequence(a, e))
    println(s"Does $a has $f as a subsequence? " + hasSubsequence(a, f))
  }
}

object Trees {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  val tree = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))
}

object Ex3dot25 {
  /*
   * Write a function size that counts the number of nodes (leaves and branches) in a
   * tree.
   */
  import Trees.{Tree, Branch, Leaf, tree}

  def size[A](t: Tree[A]): Int =
    t match {
      case Branch(left, right) => 1 + size(left) + size(right)
      case Leaf(_)             => 1
    }

  def main(): Unit = {
    println("# Exercise 3.25")
    val treeSize = size(tree)
    println(s"The tree size is $treeSize.")
  }
}

object Ex3dot26 {
  /*
   * Write a function `maximum` that returns the maximum element in a Tree[Int].
   */
  import Trees.{Tree, Branch, Leaf, tree}

  def maximum(t: Tree[Int]): Int =
    t match {
      case Branch(left, right) => maximum(left) max maximum(right)
      case Leaf(value)         => value
    }

  def main(): Unit = {
    println("# Exercise 3.26")
    val maxElement = maximum(tree)
    println(s"The maximum element in the tree is $maxElement.")
  }
}

object Ex3dot27 {
  /*
   * Write a function `depth` that returns the maximum path length from the root of a
   * tree to any leaf.
   */
  import Trees.{Tree, Branch, Leaf, tree}

  def depth[A](t: Tree[A]): Int =
    t match {
      case Branch(left, right) => 1 + (depth(left) max depth(right))
      case Leaf(value)         => 0
    }

  def main(): Unit = {
    println("# Exercise 3.27")
    val treeDepth = depth(tree)
    println(s"The tree depth is $treeDepth.")
  }
}

object Ex3dot28 {
  /*
   * Write a function `map`, analogous to the method of the same name on `List`, that
   * modifies each element in a tree with a given function.
   */
  import Trees.{Tree, Branch, Leaf, tree}

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }

  def main(): Unit = {
    println("# Exercise 3.28")
    val modifiedTree = map(tree)(_ * 10)
    println(s"The modified tree is $modifiedTree.")
  }
}

object Ex3dot29 {
  /*
   * Generalise `size`, `maximum`, `depth`, and `map`, writing a new function `fold`
   * that abstracts over their similarities.
   */
  import Trees.{Tree, Branch, Leaf, tree}

  def fold[A, B](t: Tree[A])(f2: A => B)(f1: (B, B) => B): B =
    t match {
      case Branch(left, right) => f1(fold(left)(f2)(f1), fold(right)(f2)(f1))
      case Leaf(x)             => f2(x)
    }

  def main(): Unit = {
    println("# Exercise 3.29")
    val treeSize = fold(tree)(x => 1)((a, b) => 1 + a + b)
    println(s"The tree size is $treeSize.")

    val maxElement = fold(tree)(x => x)((a, b) => a max b)
    println(s"The maximum element in the tree is $maxElement.")

    val treeDepth = fold(tree)(x => 0)((a, b) => 1 + a max b)
    println(s"The tree depth is $treeDepth.")

    val modifiedTree =
      fold(tree)(x => Leaf(x * 10): Tree[_])((a, b) => Branch(a, b))
    println(s"The modified tree is $modifiedTree.")
  }
}

object Main extends App {
  Ex3dot2.main
  Ex3dot3.main
  Ex3dot4.main
  Ex3dot5.main
  Ex3dot6.main
  Ex3dot7.main
  Ex3dot8.main
  Ex3dot9.main
  Ex3dot10.main
  Ex3dot11.main
  Ex3dot12.main
  Ex3dot14.main
  Ex3dot15.main
  Ex3dot16.main
  Ex3dot17.main
  Ex3dot18.main
  Ex3dot19.main
  Ex3dot20.main
  Ex3dot21.main
  Ex3dot22.main
  Ex3dot23.main
  Ex3dot24.main
  Ex3dot25.main
  Ex3dot26.main
  Ex3dot27.main
  Ex3dot28.main
  Ex3dot29.main
}
