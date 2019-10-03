package chapter12

import chapter8.Gen
import chapter8.Prop
import org.scalatest.FreeSpec

class Chapter12Test extends FreeSpec {

  private val intGen = Gen.choose(-2000000, 2000000)
  private val booleanGen = Gen.boolean
  private def validationGen[A](gen: Gen[A]): Gen[Validation[String, A]] = booleanGen.flatMap { b =>
    if (b) gen.map(Validation.Success(_))
    else Gen.unit(Validation.Failure("random string"))
  }

  "validation" - {
    "success" in {
      val a = Validation.Success(1)
      val b = Validation.Success(2)
      val r = Applicative.validationApplicative[String].map2(a, b)(_ + _)
      assert(r == Validation.Success(3))
    }

    "success/failure" in {
      val a = Validation.Success(1)
      val b: Validation[String, Int] = Validation.Failure("bad")
      val r = Applicative.validationApplicative[String].map2(a, b)(_ + _)
      assert(r == b)
    }

    "failure/success" in {
      val a: Validation[String, Int] = Validation.Failure("bad")
      val b = Validation.Success(1)
      val r = Applicative.validationApplicative[String].map2(a, b)(_ + _)
      assert(r == a)
    }

    "failure" in {
      val a: Validation[String, Int] = Validation.Failure("bad1", Vector("bad2"))
      val b: Validation[String, Int] = Validation.Failure("bad3", Vector("bad4"))
      val r = Applicative.validationApplicative[String].map2(a, b)(_ + _)
      assert(r == Validation.Failure("bad1", Vector("bad2", "bad3", "bad4")))
    }

    "laws" in {
      val prop = Applicative.applicativeLaws(
        Applicative.validationApplicative[String],
        validationGen(intGen),
        (i: Int) => i + 5,
        (j: Int) => j + 10
      )
      Prop.run(prop)
    }
  }
}
