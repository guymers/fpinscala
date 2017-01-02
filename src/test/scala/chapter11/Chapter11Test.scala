package chapter11

import chapter6.State
import org.scalatest.{EitherValues, FlatSpec}

class Chapter11Test extends FlatSpec with EitherValues {

  "monad" should "replicateM" in {
//    val a = Monad.listMonad.replicateM(5, List(1, 2, 3))
//    println(a)
  }

  "monad" should "filterM" in {
    val ans = Monad.listMonad.filterM(List(1, 2, 3))(a => if (a != 2) List(true, true) else List(false))
    assert(ans === List(List(1, 3), List(1, 3), List(1, 3), List(1, 3)))
  }

  // 11.18
  "state monad" should "replicateM" in {
    val s = State.unit[Int, Int](5)

    val replicate = Monad.stateMonad.replicateM(5, s)
    val ans = replicate.run(0)
    assert(ans._1 === List(5, 5, 5, 5, 5))
  }

  "state monad" should "sequence" in {
    val s = List(
      State.unit[Int, Int](1),
      State.unit[Int, Int](2),
      State.unit[Int, Int](3)
    )

    val seq = Monad.stateMonad.sequence(s)
    val ans = seq.run(0)
    assert(ans._1 === List(1, 2, 3))
  }
}
