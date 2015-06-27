import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    // 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("cannot get the tail of an empty list")
      case Cons(_, xs) => xs
    }

    // 3.3
    def setHead[A](l: List[A], head: A): List[Any] = l match {
      case Nil => sys.error("cannot set the head of an empty list")
      case Cons(_, xs) => Cons(head, xs)
    }

    // 3.4
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }

    // 3.5
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

    // 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("list is empty")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    // 3.9
    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, length) => length + 1)

    // 3.10
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // 3.11
    def suml(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
    def productl(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
    def lengthl[A](as: List[A]): Int = foldLeft(as, 0)((length, _) => length + 1)

    // 3.12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((ll, head) => Cons(head, ll))

    // 3.13
    def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      foldRightL(reverse(as), z)((a: A, b: B) => f(b, a))
    }

    def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
    }

    // 3.14
    def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

    // 3.15
    def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

    // 3.16
    def intPlus1(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((head, ll) => Cons(head + 1, ll))

    // 3.17
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((head, ll) => Cons(head.toString, ll))

    // 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((x, l) => Cons(f(x), l))

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((x, l) => if (f(x)) Cons(x, l) else l)

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, Nil: List[B])((x, l) => append(f(x), l))

    // 3.21
    def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) List(a) else Nil)

    // 3.22

    // 3.23
    def zipWith[A, B, C](l1: List[A], l2: List[B], f: (A, B) => C): List[C] = {
      @tailrec
      def go(l1: List[A], l2: List[B], acc: List[C]): List[C] = (l1, l2) match {
        case (Nil, _) | (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, append(acc, List(f(x, y))))
      }
      go(l1, l2, Nil)
    }

    // 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def hasSubseq(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, xs), Cons(y, ys)) => x == y && hasSubseq(xs, ys)
      }

      @tailrec
      def go(l: List[A]): Boolean = l match {
        case Nil => false
        case Cons(x, xs) => hasSubseq(l, sub) || go(xs)
      }
      go(sup)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    // 3.25
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    // 3.26
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }

    // 3.27
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(value) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

    // 3.28
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    // 3.29
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

    def sizeF[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
    def maximumF(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)
    def depthF[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((d1: Int, d2: Int) => 1 + (d1 max d2))
  }
}
