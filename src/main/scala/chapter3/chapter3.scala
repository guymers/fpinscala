package chapter3

import scala.annotation.tailrec

// 3.1 -> 3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def empty[A]: List[A] = Nil

  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B): B = ls match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // 3.2
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // 3.4
  @tailrec def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case l if n <= 0 => l
    case Nil => Nil
    case Cons(_, t) => drop(t, n - 1)
  }

  // 3.5
  @tailrec def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Nil => Nil
    case l @ Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // 3.6
  def init[A](ls: List[A]): List[A] = ls match {
    case Nil | Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // 3.7 -> no
  // 3.8 -> same as input List

  // 3.9
  def length[A](ls: List[A]): Int = {
    foldRight(ls, 0) { case (_, length) => length + 1 }
  }

  // 3.10
  @tailrec def foldLeft[A, B](ls: List[A], z: B)(f: (B, A) => B): B = ls match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.11
  def sumL(ls: List[Int]): Int = foldLeft(ls, 0)(_ + _)
  def productL(ls: List[Double]): Double = foldLeft(ls, 1.0)(_ * _)
  def lengthL[A](ls: List[A]): Int = foldLeft(ls, 0)((l, _) => l + 1)

  // 3.12
  def reverse[A](ls: List[A]): List[A] = {
    foldLeft(ls, List.empty[A]) { case (t, h) => Cons(h, t) }
  }

  // 3.13
  def foldLeftRight[A, B](ls: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(ls), z)((a, b) => f(b, a))
  }

  def foldRightLeft[A, B](ls: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(ls), z)((b, a) => f(a, b))
  }

  // 3.14
  def append[A](ls1: List[A], ls2: List[A]): List[A] = {
    foldRight(ls1, ls2)(Cons(_, _))
  }

  // 3.15
  def concat[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, List.empty[A])(append)
  }

  // 3.16
  def intPlus1(ls: List[Int]): List[Int] = {
    foldRight(ls, List.empty[Int]) { case (i, l) => Cons(i + 1, l) }
  }

  // 3.17
  def doubleToString(ls: List[Double]): List[String] = {
    foldRight(ls, List.empty[String]) { case (d, l) => Cons(d.toString, l) }
  }

  // 3.18
  def map[A, B](ls: List[A])(f: A => B): List[B] = {
    foldRight(ls, List.empty[B]) { case (v, l) => Cons(f(v), l) }
  }

  // 3.19
  def filter[A](ls: List[A])(f: A => Boolean): List[A] = {
    foldRight(ls, List.empty[A]) { case (v, l) =>
      if (f(v)) Cons(v, l) else l
    }
  }

  // 3.20
  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = {
    concat(map(ls)(f))
  }

  def flatMapA[A, B](ls: List[A])(f: A => List[B]): List[B] = {
    foldRight(ls, List.empty[B])((a, l) => append(f(a), l))
  }

  // 3.21
  def filterFlatMap[A](ls: List[A])(f: A => Boolean): List[A] = {
    flatMap(ls)(a => if (f(a)) List(a) else Nil)
  }

  // 3.22
  def zipPlus(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, zipPlus(as, bs))
  }

  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
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
      case Cons(_, xs) => hasSubseq(l, sub) || go(xs)
    }
    go(sup)
  }
}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximumFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0) { case (l, r) => 1 + (l max r) }
  }

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(v => Leaf(f(v)) : Tree[B])(Branch(_, _))
  }
}
