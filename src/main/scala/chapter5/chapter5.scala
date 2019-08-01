package chapter5

import scala.annotation.tailrec

trait Stream[+A] {
  import Stream._

  // 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  } // ANS: extra step for n == 0 to avoid calling take on the empty stream

  @tailrec final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case s => s
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, bool) => p(a) && bool)
  }

  // 5.5
  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { case (a, s) =>
      if (p(a)) cons(a, s) else empty
    }
  }

  // 5.6
  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B]) { case (a, tail) =>
      cons(f(a), tail)
    }
  }

  def filter[B](p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { case (a, tail) =>
      if (p(a)) cons(a, tail) else tail
    }
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)(cons(_, _))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]) { case (a, tail) =>
      f(a).append(tail)
    }
  }

  // 5.13
  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(() => this) { s =>
      s() match {
        case Empty => None
        case Cons(h, t) => Some((f(h()), t))
      }
    }
  }

  def takeUnfold(n: Int): Stream[A] = {
    unfold((n, () => this)) { case (n, s) =>
      s() match {
        case Cons(h, t) if n > 0 => Some((h(), (n - 1, t)))
        case _ => None
      }
    }
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    unfold(() => this) { s =>
      s() match {
        case Cons(h, t) if p(h()) => Some((h(), t))
        case _ => None
      }
    }
  }

  def zipWith[B, C](that: => Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((() => this, () => that)) { case (as, bs) =>
      (as(), bs()) match {
        case (Cons(a, ta), Cons(b, tb)) => Some((f(a(), b()), (ta, tb)))
        case _ => None
      }
    }
  }

  def zipAll[B](that: => Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((() => this, () => that)) { case (as, bs) =>
      (as(), bs()) match {
        case (Cons(a, ta), Cons(b, tb)) => Some(((Some(a()), Some(b())), (ta, tb)))
        case (Cons(a, ta), Empty) => Some(((Some(a()), None), (ta, () => Empty)))
        case (Empty, Cons(b, tb)) => Some(((None, Some(b())), (() => Empty, tb)))
        case _ => None
      }
    }
  }

  // 5.14
  def startsWith[B >: A](that: => Stream[B]): Boolean = {
    zipAll(that).takeWhile(_._2.isDefined).forAll { case (a, b) => a == b }
  }

  // 5.15
  def tails: Stream[Stream[A]] = {
    unfold(() => this) { s =>
      s() match {
        case Empty => None
        case s @ Cons(_, tail) => Some((s, tail))
      }
    }.append(apply(empty))
  }

  // 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    // this isnt linear :/
    this match {
      case s @ Cons(_, t) => cons(s.foldRight(z)(f), t().scanRight(z)(f))
      case Empty => apply(z)
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = cons(1, ones)

  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = cons(a, s)
    s
  }

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  val fibs: Stream[Int] = {
    def go(num: Int, prev: Int, cur: Int): Stream[Int] = {
      cons(cur, go(num + 1, cur, cur + prev))
    }
    cons(0, go(1, 0, 1))
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  // 5.12
  val onesUnfold: Stream[Int] = unfold(())(_ => Some((1, ())))

  def constantUnfold[A](a: A): Stream[A] = unfold(())(_ => Some((a, ())))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  val fibsUnfold: Stream[Int] = {
    unfold((0, 0)) { case (prev, cur) =>
      val next = if (cur == 0) 1 else prev + cur
      Some((cur, (cur, next)))
    }
  }
}
