package chapter7

import java.util.concurrent.Executors

import org.scalatest.FlatSpec

class Chapter7Test extends FlatSpec {

  val es = Executors.newCachedThreadPool

  "blocking" should "sequence" in {
    val a = List(Par.unit(1), Par.unit(2), Par.unit(3))
    val s = Par.sequence(a)
    val r = Par.run(es)(s)
    assert(r.get == List(1, 2, 3))
  }

  "blocking" should "parFilter" in {
    val a = List(Par.unit(1), Par.unit(2), Par.unit(3))
    val s = Par.parFilter(List(1, 2, 3, 4))(_ % 2 == 0)
    val r = Par.run(es)(s)
    assert(r.get == List(2, 4))
  }

  "nonblocking" should "parMap" in {
    val p = Nonblocking.Par.parMap(List.range(1, 5))(_ * 2)
    val r = Nonblocking.Par.run(es)(p)

    assert(r == List(2, 4, 6, 8))
  }

  "nonblocking" should "rethrow exceptions" in {
    val l = List(1, 2, 3)
    val p = Nonblocking.Par.lazyUnit {
      l(4)
    }
    intercept[IndexOutOfBoundsException] {
      val a = Nonblocking.Par.run(es)(p)
      println(a)
    }
    ()
  }

  "blocking" should "choiceN" in {
    val a = List(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4))
    val s = Par.choiceN(Par.unit(2))(a)
    val r = Par.run(es)(s)
    assert(r.get == 3)
  }

  "blocking" should "choice2" in {
    def choose(b: Boolean, expected: Int): Unit = {
      val s = Par.choice2(Par.unit(b))(Par.unit(7), Par.unit(5))
      val r = Par.run(es)(s)
      assert(r.get == expected)
    }
    choose(b = true, 7)
    choose(b = false, 5)
  }

  "blocking" should "chooser" in {
    val s = Par.choiceC(Par.unit(true))(Par.unit(7), Par.unit(5))
    val r = Par.run(es)(s)
    assert(r.get == 7)

    val a2 = List(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4))
    val s2 = Par.choiceNC(Par.unit(2))(a2)
    val r2 = Par.run(es)(s2)
    assert(r2.get == 3)
  }
}
