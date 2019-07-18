package chapter4

import org.scalatest.FreeSpec

class Chapter4Test extends FreeSpec {

  "option" - {
    "map" in {
      assert(Some(5).map(_ + 1) == Some(6))
      assert((None : Option[Int]).map(_ + 1) == None)
    }

    "flatMap" in {
      assert(Some(5).flatMap(v => Some(v + 1)) == Some(6))
      assert(Some(5).flatMap(_ => None) == None)
      assert((None : Option[Int]).flatMap(v => Some(v + 1)) == None)
    }

    "getOrElse" in {
      assert(Some(5).getOrElse(1) == 5)
      assert((None : Option[Int]).getOrElse(1) == 1)
    }

    "orElse" in {
      assert(Some(5).orElse(None) == Some(5))
      assert((None : Option[Int]).orElse(Some(1)) == Some(1))
      assert((None : Option[Int]).orElse(None) == None)
    }

    "filter" in {
      assert(Some(5).filter(_ == 5) == Some(5))
      assert(Some(5).filter(_ != 5) == None)
      assert((None : Option[Int]).filter(_ == 5) == None)
    }

    "variance" in {
      val xs = Seq(600.0, 470.0, 170.0, 430.0, 300.0)
      assert(Option.variance(xs) == Some(21704))
    }

    "map2" in {
      assert(Option.map2(Some(4), Some(6))(_ + _) == Some(10))
      assert(Option.map2[Int, Int, Int](None, Some(6))(_ + _) == None)
      assert(Option.map2[Int, Int, Int](Some(4), None)(_ + _) == None)
      assert(Option.map2[Int, Int, Int](None, None)(_ + _) == None)
    }

    "sequence" in {
      assert(Option.sequence(List(Some(4), Some(6), Some(8))) == Some(List(4, 6, 8)))
      assert(Option.sequence(List(Some(4), None, Some(8))) == None)

      assert(Option.sequenceT(List(Some(4), Some(6), Some(8))) == Some(List(4, 6, 8)))
      assert(Option.sequenceT(List(Some(4), None, Some(8))) == None)
    }

    "traverse" in {
      assert(Option.traverse(List(1, 2, 3, 4))(Some(_)) == Some(List(1, 2, 3, 4)))
      assert(Option.traverse(List(1, 2, 3, 4))(a => if (a > 3) None else Some(a)) == None)
    }
  }

  "either" - {
    def left[E](l: E): Either[E, Int] = Left(l)

    "map" in {
      assert(Right(5).map(_ + 1) == Right(6))
      assert(left("").map(_ + 1) == Left(""))
    }

    "flatMap" in {
      assert(Right(5).flatMap(v => Right(v + 1)) == Right(6))
      assert(Right(5).flatMap(_ => Left("")) == Left(""))
      assert(left("").flatMap(v => Right(v + 1)) == Left(""))
    }

    "orElse" in {
      assert(Right(5).orElse(Left("")) == Right(5))
      assert(Left("").orElse(Right(1)) == Right(1))
      assert(Left("").orElse(Left("e")) == Left("e"))
    }

    "map2" in {
      assert(Right(5).map2(Right(1))(_ + _) == Right(6))
      assert(Right(5).map2(left(""))(_ + _) == Left(""))
      assert(left("").map2(Right(5))(_ + _) == Left(""))
      assert(left("").map2(left("a"))(_ + _) == Left(""))
    }

    "sequence" in {
      assert(Either.sequence(List(Right(4), Right(6), Right(8))) == Right(List(4, 6, 8)))
      val e = new Exception
      assert(Either.sequence(List(Right(4), Left(e), Right(8))) == Left(e))
    }

    "traverse" in {
      assert(Either.traverse(List(1, 2, 3, 4))(Right(_)) == Right(List(1, 2, 3, 4)))
      val e = new Exception
      assert(Either.traverse(List(1, 2, 3, 4))(a => if (a > 3) Left(e) else Right(a)) == Left(e))
    }
  }
}
