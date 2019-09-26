package chapter11

import chapter6.State
import chapter8.Gen
import chapter8.Prop
import org.scalatest.FreeSpec

class Chapter11Test extends FreeSpec {

  private val intGen = Gen.choose(-2000000, 2000000)
  private val booleanGen = Gen.boolean
  private def optionGen[A](gen: Gen[A]) = (booleanGen ** booleanGen ** gen).map { case ((b1, b2), g) =>
    if (!b1 && !b2) None else Some(g)
  }

  "monad" - {
    "laws" in {
      val prop = Monad.monadLaws(Monad.optionMonad, optionGen(intGen), (i: Int) => i + 5, (j: Int) => j + 10)
      Prop.run(prop)
    }

    "replicateM" in {
      // val a = Monad.listMonad.replicateM(5, List(1, 2, 3))
      val none = Monad.optionMonad.replicateM(3, None)
      val some = Monad.optionMonad.replicateM(3, Some(5))
      assert(none == None)
      assert(some == Some(List(5, 5, 5)))
    }

    "filterM" in {
      val ans = Monad.listMonad.filterM(List(1, 2, 3))(a => if (a != 2) List(true, true) else List(false))
      assert(ans == List(List(1, 3), List(1, 3), List(1, 3), List(1, 3)))
    }
  }

  // 11.18
  "state monad" - {
    "replicateM" in {
      val s = State.unit[Int, Int](5)

      val replicate = Monad.stateMonad.replicateM(5, s)
      val (ans, _) = replicate.run(0)
      assert(ans == List(5, 5, 5, 5, 5))
    }

    "sequence" in {
      val s = List(
        State.unit[Int, Int](1),
        State.unit[Int, Int](2),
        State.unit[Int, Int](3)
      )

      val seq = Monad.stateMonad.sequence(s)
      val (ans, _) = seq.run(0)
      assert(ans == List(1, 2, 3))
    }
  }
}
