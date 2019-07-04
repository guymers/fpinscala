package chapter8

import chapter6.{RNG, State}
import org.scalatest.FlatSpec

class Chapter8Test extends FlatSpec {

  "gen" should "choose" in {
    val rng = RNG.Simple(1234L)
    val s = Gen.choose(5, 10).sample

    val list = List.fill(50)(s)
    val r = State.sequence(list).run(rng)
    assert(r._1.forall(n => n >= 5 && n < 10))
  }

  "gen" should "unit" in {
    val rng = RNG.Simple(1234L)
    val s = Gen.unit(5).sample

    val list = List.fill(50)(s)
    val r = State.sequence(list).run(rng)
    val grouped = r._1.groupBy(identity)
    assert(grouped(5).length === 50)
  }

  "gen" should "boolean" in {
    val rng = RNG.Simple(1234L)
    val s = Gen.boolean.sample

    val list = List.fill(50)(s)
    val r = State.sequence(list).run(rng)
    val grouped = r._1.groupBy(identity).view.mapValues(_.length).toMap
    println(grouped)
  }

  "gen" should "listOfN" in {
    val rng = RNG.Simple(1234L)
    val gen = Gen.listOfN(50, Gen.choose(5, 10))

    val r = gen.sample.run(rng)
    assert(r._1.forall(n => n >= 5 && n < 10))
  }

  "gen" should "listOfN dynamic" in {
    val rng = RNG.Simple(1234L)
    val size = Gen.choose(10, 40)
    val gen = Gen.choose(5, 10).listOfN(size)

    val r = gen.sample.run(rng)
    assert(r._1.forall(n => n >= 5 && n < 10))

    val grouped = r._1.groupBy(identity).view.mapValues(_.length).toMap
    println(grouped)
  }

  "gen" should "union" in {
    val rng = RNG.Simple(1234L)
    val g1 = Gen.choose(5, 10)
    val g2 = Gen.choose(25, 30)
    val s = Gen.union(g1, g2).sample

    val list = List.fill(50)(s)
    val r = State.sequence(list).run(rng)
    val grouped = r._1.groupBy(identity).view.mapValues(_.length).toMap
    println(grouped)
  }

  "gen" should "weighted" in {
    val rng = RNG.Simple(1234L)
    val g1 = Gen.choose(5, 10)
    val g2 = Gen.choose(25, 30)
    val gen = Gen.weighted((g1, 1.3), (g2, 4.3))

    val list = List.fill(50)(gen.sample)
    val r = State.sequence(list).run(rng)
    val grouped = r._1.groupBy(identity).view.mapValues(_.length).toMap
    println(grouped)
  }

  "gen" should "maxProp empty fails" in {
//    val smallInt = Gen.choose(-10, 10)
//    val maxProp = SGen.forAll(SGen.listOf(smallInt)) { ns =>
//      val max = ns.max
//      !ns.exists(_ > max)
//    }
    //Prop.run(maxProp)
  }

  "gen" should "maxProp nonempty list" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = SGen.forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  // 8.14
  "prop" should "be able to verify the behaviour of List.sorted" in {
    val order = Ordering.Int

    val smallInt = Gen.choose(-10, 10)
    val maxProp = SGen.forAll(SGen.listOf1(smallInt)) { ns =>
      val min = if (ns.isEmpty) None else Some(ns.min(order))

      val sorted = ns.sorted(order)
      val sortedHead = sorted.headOption

      val minMatches = (for {
        m <- min
        head <- sortedHead
      } yield m == head).getOrElse(false)

      sorted.foldLeft[(Option[Int], Boolean)]((None, minMatches)) {
        case ((previous, okay), value) =>
          (Some(value), okay && previous.forall(n => order.gteq(value, n)))
      }._2
    }
    Prop.run(maxProp)
  }

  // 8.18
  // val ls = List
  // ls.sorted == (list.takeWhile + list.dropWhile).sorted
}
