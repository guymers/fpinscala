package chapter10

import chapter8.{Gen, Prop}

trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}

object Monoid {

  val stringConcatenation: Monoid[String] = new Monoid[String] {
    override val zero: String = ""
    override def op(a1: String, a2: String): String = a1 concat a2
  }

  def listConcatenation[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override val zero: List[A] = Nil
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2
  }

  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override val zero: Int = 0
    override def op(a1: Int, a2: Int): Int = a1 + a2
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override val zero: Int = 1
    override def op(a1: Int, a2: Int): Int = a1 * a2
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override val zero: Boolean = false
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override val zero: Boolean = true
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override val zero: Option[A] = None
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  // 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override val zero: A => A = identity
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
  }

  // 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A])(implicit eq: (A, A) => Boolean = (a1: A, a2: A) => a1 == a2): Prop = {
    val leftIdentity = Prop.forAll(gen) { a =>
      eq(m.op(m.zero, a), a)
    }

    val rightIdentity = Prop.forAll(gen) { a =>
      eq(m.op(a, m.zero), a)
    }

    val associativity = Prop.forAll(gen ** gen ** gen) { case ((a, b), c) =>
      eq(m.op(m.op(a, b), c), m.op(a, m.op(b, c)))
    }

    leftIdentity && rightIdentity && associativity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // 10.5
  def foldMap[A, B](as: Seq[A], m: Monoid[B])(f: A => B): B = {
    // concatenate(as.map(f), m)
    as.foldLeft(m.zero) { case (b, a) => m.op(b, f(a)) }
  }

  // 10.6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  // 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val length = v.length
    if (length <= 0) {
      m.zero
    } else if (length == 1) {
      f(v.head) // identity law
    } else {
      val (l, r) = v.splitAt(length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // 10.8
  import chapter7.nonblocking._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override val zero: Par[A] = Par.unit(m.zero)
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    //Par.parMap(v)(f).flatMap { bs =>
    //  foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    //}
    ???
  }

  // 10.9
  type PreviousValue[A] = (Boolean, Option[A])
  val sortedIntMonoid: Monoid[PreviousValue[Int]] = new Monoid[PreviousValue[Int]] {
    override val zero: PreviousValue[Int] = (true, None)
    override def op(a1: PreviousValue[Int], a2: PreviousValue[Int]): PreviousValue[Int] = {
      val sorted = for {
        v1 <- a1._2
        v2 <- a2._2
      } yield {
        a1._1 && v1 < v2
      }
      (sorted.getOrElse(true), a2._2)
    }
  }
  def isSorted(v: IndexedSeq[Int]): Boolean = {
    foldMap(v.toList, sortedIntMonoid)(v => (false, Some(v)))._1
  }

  // 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override val zero: WC = Stub("")
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (Stub(c1), Stub(c2)) => Stub(c1 concat c2)
        case (Stub(c), Part(l, count, r)) => Part(c concat l, count, r)
        case (Part(l, count, r), Stub(c)) => Part(l, count, r concat c)
        case (Part(l, count1, m1), Part(m2, count2, r)) =>
          val count = count1 + count2 + (if (m1.nonEmpty || m2.nonEmpty) 1 else 0)
          Part(l, count, r)
      }
    }
  }

  // 10.11
  def countWords(s: String): Int = {
    def wc(c: Char): WC = {
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }

    def count(s: String): Int = if (s.nonEmpty) 1 else 0
    foldMapV(s, wcMonoid)(wc) match {
      case Stub(s) => count(s)
      case Part(l, words, r) => count(l) + words + count(r)
    }
  }

  // 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override val zero: (A, B) = (A.zero, B.zero)
    override def op(a1: (A, B), a2: (A, B)): (A, B) = {
      (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    }
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override val zero: Map[K, V] = Map.empty
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(
          k,
          V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero))
        )
      }
    }
  }

  // 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override val zero: A => B = _ => B.zero
    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
  }

  // 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    foldMap(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
  }
}

sealed trait WC extends Product with Serializable
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC


trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero) { case (acc, v) => mb.op(acc, f(v)) }
  }
  def concatenate[A](as: F[A])(m: Monoid[A]): A = {
    foldLeft(as)(m.zero)(m.op)
  }

  // 10.15
  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List.empty[A])(_ :: _)
  }
}

object Foldable {
  import chapter3.Tree

  // 10.12
  val list = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  val indexSeq = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  val lazyList = new Foldable[LazyList] {
    override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  // 10.13
  // ANS: new function for each using pattern matching
  val tree = new Foldable[Tree] {
    override def foldRight[A, B](t: Tree[A])(z: B)(f: (A, B) => B): B =
      Tree.fold(t)(f.curried) { case (a, b) => b andThen a } (z)
    override def foldLeft[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B =
      Tree.fold(t) { a => (b: B) => f(b, a) } { case (b, a) => a andThen b } (z)
  }

  // 10.14
  val option = new Foldable[Option] {
    override def foldRight[A, B](t: Option[A])(z: B)(f: (A, B) => B): B =
      t.map(a => f(a, z)).getOrElse(z)
    override def foldLeft[A, B](t: Option[A])(z: B)(f: (B, A) => B): B =
      t.map(a => f(z, a)).getOrElse(z)
  }

}
