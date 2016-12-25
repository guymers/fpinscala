package chapter6

import org.scalatest.FlatSpec

class Chapter6Test extends FlatSpec {

  import RNG._

  "nonNegativeInt" should "" in {
    val intMinValueRng = new RNG {
      override def nextInt: (Int, RNG) = (Int.MinValue, Simple(1))
    }
    val (r, _) = nonNegativeInt(intMinValueRng)
    assert(r != Int.MinValue)
  }

  "double" should "" in {
    val (r, _) = double(Simple(1))
    assert(r >= 0 && r < 1)

    val (r2, _) = doubleMap(Simple(1))
    assert(r2 >= 0 && r2 < 1)

    assert(r == r2)
  }

  "ints" should "" in {
    val rng: RNG = Simple(4)
    val (ints1, _) = ints(5)(rng)
    val size1 = ints1.size
    assert(size1 == 5)

    val (ints2, _) = intsSequence(5)(rng)
    val size2 = ints2.size
    assert(size2 == 5)

    assert(ints1 == ints2)
  }

  "nonNegativeLessThan" should "" in {
    val rng: RNG = Simple(10)

    val (r1, _) = nonNegativeLessThan(5)(rng)
    val (r2, _) = nonNegativeLessThanWithFlatMap(5)(rng)

    assert(r1 == r2)
  }

  "machine" should "" in {
    val buy4 = List.fill(4)(Machine.buyCandy).flatten
    val machine = Machine(locked = true, coins = 10, candies = 5)
    val result = Machine.simulateMachine(buy4).run(machine)
    assert(result._1 == ((14, 1)))
  }
}
