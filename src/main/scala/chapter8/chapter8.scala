package chapter8

import chapter5.Stream
import chapter6.{RNG, State}

// 8.1
// sum: List[Int] => Int
//   ls.reverse.sum == ls.sum
//   (i: Int) -> ls.fill(n)(i).sum == i * n
//   ls.fill(n)(0) == 0
//   ls.randomize.sum == ls.sum

// 8.2
// max: List[Int] => Int
//   ls.reverse.max == ls.max
//   ls.randomize.max == ls.max
//   ls.sort.lastOption == ls.max

// 8.3
trait Prop3 { self =>
  def check: Boolean
  def &&(p: Prop3): Prop3 = new Prop3 {
    def check: Boolean = self.check && p.check
  }
}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen {
    sample.map(f)
  }

  def map2[B,C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen {
    sample.map2(g.sample)(f)
  }

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
    sample.flatMap(a => f(a).sample)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(Gen.listOfN(_, this))
  }

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_,_))
}

object Gen {

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen {
    State(RNG.double).map { d =>
      (start + d * (stopExclusive - start)).toInt
    }
  }

  // 8.5
  def unit[A](a: => A): Gen[A] = Gen {
    State.unit(a)
  }

  def boolean: Gen[Boolean] = Gen {
    State(RNG.int).map(_ >= 0)
  }
  def boolean2: Gen[Boolean] = choose(0, 2).map(_ == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen {
    State.sequence(List.fill(n)(g.sample))
  }

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  // 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val weight = weight1.abs / (weight1.abs + weight2.abs)
    Gen(State(RNG.double)).flatMap { d =>
      if (d < weight) gen1 else gen2
    }
  }

}


object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override val isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override val isFalsified: Boolean = true
  }
  case object Proved extends Result {
    override val isFalsified: Boolean = false
  }

  def apply(f: (TestCases, RNG) => Result): Prop = Prop { (_, n, rng) => f(n, rng) }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n - 1) / max + 1
    val props = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop = props.map { p =>
      Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }
    }.toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  }
}

case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Prop.Result) {
  import Prop._

  // 8.9
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case r @ Falsified(_, _) => r
    }
  }
  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case r @ (Passed | Proved) => r
      case Falsified(_, _) => p.run(max, n, rng)
    }
  }
}

case class SGen[A](forSize: Int => Gen[A]) {

  // 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i =>
    forSize(i).flatMap(a => f(a).forSize(i))
  }

}

object SGen {

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { i =>
    Gen.listOfN(i, g)
  }

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { i =>
    Gen.listOfN(i max 1, g)
  }
}
