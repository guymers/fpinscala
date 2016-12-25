package chapter8

import java.util.concurrent.Executors

import chapter5.Stream
import chapter6.{RNG, State}
import chapter7.Par.Par
import chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

// 8.3
trait Prop3 {
  def check: Boolean
  def &&(p: Prop3): Prop3 = new Prop3 {
    def check = check && p.check
  }
}

case class Gen[A](sample: State[RNG, A]) {

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
    sample.flatMap(a => f(a).sample)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] = Gen {
    sample.map(f)
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen {
    sample.map2(g.sample)(f)
  }

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}

object Gen {

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen {
    State[RNG, Int] {
      RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))(_ + start)
    }
  }

  // 8.5
  def unit[A](a: => A): Gen[A] =
    Gen { State.unit(a) }

  def boolean: Gen[Boolean] =
    Gen { choose(0, 2).sample.map(_ == 1) }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val list = List.fill(n)(g.sample)
    Gen(State.sequence(list))
  }

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  // 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val weight = weight1 + weight2

    // make the number "doubly"
    val factor = 1000
    Gen.choose(0, (weight * factor).toInt).flatMap { n =>
      if (n < (weight1 * factor).toInt) gen1 else gen2
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case $s generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
case object Proved extends Result {
  override def isFalsified: Boolean = false
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis())
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
}

case class Prop(run: (MaxSize, TestCases, RNG) => chapter8.Result) {

  // 8.9
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case r => r
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(_, _) => p.run(max, n, rng)
      case r => r
    }
  }
}

case class SGen[A](forSize: Int => Gen[A])

object SGen {

  // 8.11
  def unit[A](a: => A): SGen[A] = Gen.unit(a).unsized

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => Gen.forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { n =>
    val size = if (n <= 0) 1 else n
    Gen.listOfN(size, g)
  }
}
