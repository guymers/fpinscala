package chapter8

import chapter6.{RNG, State}
import org.scalatest.FreeSpec

class Chapter8Test extends FreeSpec {

  "gen" - {
    "choose" in {
      val rng = RNG.Simple(1234L)
      val s = Gen.choose(5, 10).sample

      val list = List.fill(50)(s)
      val (result, _) = State.sequence(list).run(rng)
      assert(result.forall(n => n >= 5 && n < 10))
    }

    "unit" in {
      val rng = RNG.Simple(1234L)
      val s = Gen.unit(5).sample

      val list = List.fill(50)(s)
      val (result, _) = State.sequence(list).run(rng)
      val grouped = result.groupBy(identity)
      assert(grouped(5).length === 50)
    }

    "boolean" in {
      val rng = RNG.Simple(1234L)
      val s = Gen.boolean.sample

      val list = List.fill(50)(s)
      val (result, _) = State.sequence(list).run(rng)
      val grouped = result.groupBy(identity).view.mapValues(_.length).toMap
      println(grouped)
    }

    "listOfN" in {
      val rng = RNG.Simple(1234L)
      val gen = Gen.listOfN(50, Gen.choose(5, 10))

      val (result, _) = gen.sample.run(rng)
      assert(result.forall(n => n >= 5 && n < 10))
    }

    "listOfN dynamic" in {
      val rng = RNG.Simple(1234L)
      val size = Gen.choose(10, 40)
      val gen = Gen.choose(5, 10).listOfN(size)

      val (result, _) = gen.sample.run(rng)
      assert(result.forall(n => n >= 5 && n < 10))

      val grouped = result.groupBy(identity).view.mapValues(_.length).toMap
      println(grouped)
    }

    "union" in {
      val rng = RNG.Simple(1234L)
      val g1 = Gen.choose(5, 10)
      val g2 = Gen.choose(25, 30)
      val s = Gen.union(g1, g2).sample

      val list = List.fill(50)(s)
      val (result, _) = State.sequence(list).run(rng)
      val grouped = result.groupBy(identity).view.mapValues(_.length).toMap
      println(grouped)
    }

    "weighted" in {
      val rng = RNG.Simple(1234L)
      val g1 = Gen.choose(5, 10)
      val g2 = Gen.choose(25, 30)
      val gen = Gen.weighted((g1, 1.3), (g2, 4.3))

      val list = List.fill(50)(gen.sample)
      val (result, _) = State.sequence(list).run(rng)
      val grouped = result.groupBy(identity).view.mapValues(_.length).toMap
      println(grouped)
    }

    "maxProp empty fails" in {
//      val smallInt = Gen.choose(-10, 10)
//      val maxProp = SGen.forAll(SGen.listOf(smallInt)) { ns =>
//        val max = ns.max
//        !ns.exists(_ > max)
//      }
//      Prop.run(maxProp)
    }

    "maxProp nonempty list" in {
      val smallInt = Gen.choose(-10, 10)
      val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }
      Prop.run(maxProp)
    }
  }

  // 8.14
  "prop" - {
    "verify the behaviour of List.sorted" in {
      val order = Ordering.Int

      val smallInt = Gen.choose(-10, 10)
      val maxProp = Prop.forAll(SGen.listOf1(smallInt)) { ns =>
        val min = if (ns.isEmpty) None else Some(ns.min(order))

        val sorted = ns.sorted(order)
        val sortedHead = sorted.headOption

        val minMatches = (for {
          m <- min
          head <- sortedHead
        } yield m == head).getOrElse(false)

        sorted.foldLeft((None: Option[Int], minMatches)) { case ((previous, okay), value) =>
          (Some(value), okay && previous.forall(n => order.gteq(value, n)))
        }._2
      }
      Prop.run(maxProp)
    }
  }

  // 8.18
  // takeWhile: List[A] => (A => Boolean) => List[A]
  //   ls.sorted == (ls.takeWhile ::: ls.dropWhile).sorted
  //   ls.takeWhile == ls.reverse.dropWhile
}
