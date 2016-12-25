package chapter5

import scala.annotation.tailrec

  trait Stream[+A] {

    import Stream._

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    @tailrec
    final def find(f: A => Boolean): Option[A] = this match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    // 5.1
    def toList: List[A] = foldRight(List.empty[A])((a, b) => a :: b)

    // 5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons[A](h(), t().take(n - 1))
      // ANS: extra step for n == 0 to avoid calling take on the empty stream
      case _ => empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      // ANS: move if into case, catch all can return empty
      case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
      case _ => this
    }

    // 5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // 5.5
    def takeWhileR(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

    // 5.6
    def headOption: Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    // 5.7
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => f(a).append(b))

    // 5.13
    // ANS: uses pattern matching for unfold versions
    def mapUnfold[B](f: A => B): Stream[B] =
      unfold(this) { s =>
        for {
          head <- s.headOption
          cur = f(head)
        } yield (cur, s.drop(1))
      }

    def takeUnfold(n: Int): Stream[A] =
      unfold((n, this)) { case (num, s) =>
        for {
          cur <- s.headOption
          if num > 0
        } yield (cur, (num - 1, s.drop(1)))
      }

    def takeWhileUnfold(p: A => Boolean): Stream[A] =
      unfold(this) { s =>
        for {
          cur <- s.headOption
          if p(cur)
        } yield (cur, s.drop(1))
      }

    def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s)) { case (s1, s2) =>
        for {
          s1Head <- s1.headOption
          s2Head <- s2.headOption
          cur = f(s1Head, s2Head)
        } yield (cur, (s1.drop(1), s2.drop(1)))
      }

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s)) { case (s1, s2) =>
        val s1Head = s1.headOption
        val s2Head = s2.headOption
        s1Head.orElse(s2Head).map { _ =>
          def nextStreamValue[C](s: Stream[C]) = s.headOption.map(_ => s.drop(1)) getOrElse empty

          val cur = (s1Head, s2Head)
          val next = (nextStreamValue(s1), nextStreamValue(s2))
          (cur, next)
        }
      }

    def zip[B](s: Stream[B]): Stream[(A, B)] =
      zipAll(s).map { case (a, b) => (a.get, b.get) }

    // 5.14
    def startsWith[B](s: Stream[B]): Boolean =
      zipAll(s)
        .takeWhile { case (h1, h2) => h2.isDefined }
        .forAll { case (h1, h2) =>
          val equal = for {
           hh1 <- h1
           hh2 <- h2
          } yield hh1 == hh2
          equal getOrElse false
          // ANS: simplify to just h1 == h2
        }

    // 5.15
    // ANS: instead of using a boolean flag, just append the empty stream to the unfold result
    def tails: Stream[Stream[A]] =
      unfold((this, false)) { case (s, complete) =>
        if (complete) {
          None
        } else {
          Some((s, (s.drop(1), s == empty)))
        }
      }

    // 5.16
    // ANS: cannot be done with unfold because it goes from left to right
    //      use foldRight with lazy val on the result
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
      unfold((this, false)) { case (s, complete) =>
        if (complete) {
          None
        } else {
          // this is not liner time
          val result = s.foldRight(z)(f)
          Some((result, (s.drop(1), s == empty)))
        }
      }
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    // 5.8
    // ANS: lazy val the tail like ones inside the function to be more efficient
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // 5.10
    def fibs: Stream[Int] = {
      def nextNum(prev: Int, cur: Int): Stream[Int] = cons(cur, nextNum(cur, prev + cur))

      cons(0, nextNum(0, 1))
    }

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).map { case (a, s) => cons(a, unfold(s)(f)) } getOrElse empty

    // 5.12
    val onesUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

    def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

    def fibsUnfold: Stream[Int] =
      unfold((0, 0)) { case (prev, cur) =>
        val next = if (cur == 0) 1 else prev + cur
        Some((cur, (cur, next)))
      }
  }
