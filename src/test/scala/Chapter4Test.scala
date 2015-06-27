import org.scalatest.FlatSpec

class Chapter4Test extends FlatSpec {

  import Chapter4._

  "option variance" should "" in {
    val xs = Seq(600.0, 470.0, 170.0, 430.0, 300.0)
    assert(Option.variance(xs) == Some(21704))
  }

  "option map2" should "" in {
    assert(Option.map2(Some(4), Some(6))(_ + _) == Some(10))
    assert(Option.map2[Int, Int, Int](None, Some(6))(_ + _) == None)
    assert(Option.map2[Int, Int, Int](Some(4), None)(_ + _) == None)
    assert(Option.map2[Int, Int, Int](None, None)(_ + _) == None)
  }

  "option sequence" should "" in {
    assert(Option.sequence(List(Some(4), Some(6), Some(8))) == Some(List(4, 6, 8)))
    assert(Option.sequence(List(Some(4), None, Some(8))) == None)
  }

  "option traverse" should "" in {
    assert(Option.traverse(List(1, 2, 3, 4))(Some(_)) == Some(List(1, 2, 3, 4)))
    assert(Option.traverse(List(1, 2, 3, 4))(a => if (a > 3) None else Some(a)) == None)
  }

  "either sequence" should "" in {
    assert(Either.sequence(List(Right(4), Right(6), Right(8))) == Right(List(4, 6, 8)))
    val e = new Exception
    assert(Either.sequence(List(Right(4), Left(e), Right(8))) == Left(e))
  }

  "either traverse" should "" in {
    assert(Either.traverse(List(1, 2, 3, 4))(Right(_)) == Right(List(1, 2, 3, 4)))
    val e = new Exception
    assert(Either.traverse(List(1, 2, 3, 4))(a => if (a > 3) Left(e) else Right(a)) == Left(e))
  }
}
