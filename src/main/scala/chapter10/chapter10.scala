package chapter10

import chapter8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringConcatenation: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  // 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    override def zero: A => A = identity
  }

  // 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val leftIdentity = Gen.forAll(gen) { x =>
      m.op(x, m.zero) == x
    }
    val rightIdentity = Gen.forAll(gen) { x =>
      m.op(m.zero, x) == x
    }
    // ANS: flatmap instead of product
    val tripleGen = (gen ** gen ** gen).map(v => (v._1._1, v._1._2, v._2))
    val associative = Gen.forAll(tripleGen) { case (x, y, z) =>
      m.op(m.op(x, y), z) == m.op(x, m.op(y, z))
    }
    leftIdentity && rightIdentity && associative
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { case (acc, v) => m.op(acc, f(v)) }

  // 10.6
  // ANS: currying B => A => B
  def foldLeft[A, B](as: List[A], m: Monoid[B])(z: B)(op: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => op(b, a))(z)

  // 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val len = v.length
    len match {
      case 0 => m.zero
      case 1 => f(v(0))
      case _ =>
        val (a, b) = v.splitAt(len / 2)
        m.op(foldMapV(a, m)(f), foldMapV(b, m)(f))
    }
  }

  // 10.8

  // 10.9
  // ANS: tracks max and min
  type PreviousValue[A] = (Boolean, Option[A])
  val sortedIntMonoid: Monoid[PreviousValue[Int]] = new Monoid[PreviousValue[Int]] {
    override def op(a1: PreviousValue[Int], a2: PreviousValue[Int]): PreviousValue[Int] = {
      val sorted = for {
        v1 <- a1._2
        v2 <- a2._2
      } yield {
        a1._1 && v1 < v2
      }
      (sorted.getOrElse(true), a2._2)
    }
    override def zero: PreviousValue[Int] = (true, None)
  }
  def isSorted(v: IndexedSeq[Int]): Boolean =
    foldMap(v.toList, sortedIntMonoid)(v => (false, Some(v)))._1

  // 10.10
  // ANS: assumes individual characters which simplifies
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) =>
        toWC(c1 + c2)
      case (Stub(c1), Part(lStub, words, rStub)) =>
        if (c1.isEmpty || lStub.isEmpty) {
          Part(c1 + lStub, words, rStub)
        } else {
          op(toWC(c1 + lStub), Part("", words, rStub))
        }
      case (Part(lStub, words, rStub), Stub(c2)) =>
        if (c2.isEmpty || rStub.isEmpty) {
          Part(lStub, words, rStub + c2)
        } else {
          op(Part(lStub, words, ""), toWC(rStub + c2))
        }
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        val middleWord = if (!rStub1.isEmpty || !lStub2.isEmpty) 1 else 0
        Part(lStub1, words1 + words2 + middleWord, rStub2)
    }
    override def zero: WC = Stub("")
  }

  def toWC(str: String): WC = {
    if (str.isEmpty) {
      Stub(str)
    } else {
      val whitespace = """\s"""
      val leadingWhitespace = str(0).toString.matches(whitespace)
      val trailingWhitespace = str(str.length - 1).toString.matches(whitespace)

      val parts = str.split("""\s+""").filter(!_.isEmpty)
      val words = parts.length
      if (words > 1 || (words == 1 && (leadingWhitespace || trailingWhitespace))) {
        val firstPart = parts(0)
        val (lStub, lWord) = if (leadingWhitespace) ("", 0) else (firstPart, 1)
        val lastPart = parts(words - 1)
        val (rStub, rWord) = if (trailingWhitespace) ("", 0) else (lastPart, 1)
        Part(lStub, words - lWord - rWord, rStub)
      } else {
        Stub(str)
      }
    }
  }

  // 10.11
  def countWords(s: String): Int =
    foldMapV(s, wcMonoid)(c => toWC(c.toString)) match {
      case Stub(_) => 0
      case Part(_, words, _) => words
    }

  // 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) =
      (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(
          k,
          V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero))
        )
      }
  }

  // 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B =
      a => B.op(a1(a), a2(a))
    override def zero: A => B = a => B.zero
  }

  // 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m = mapMergeMonoid[A, Int](intAddition)
    foldMap(as.toList, m)(a => Map(a -> 1))
  }

}

sealed trait WC extends Product with Serializable
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC


trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { case (acc, v) => mb.op(acc, f(v)) }
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // 10.15
  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A]) { case (a, b) => a :: b }
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

  val stream = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
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
