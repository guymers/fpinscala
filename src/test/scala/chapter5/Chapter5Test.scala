package chapter5

import org.scalatest.FlatSpec

class Chapter5Test extends FlatSpec {

  "toList" should "" in {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
  }

  "take" should "" in {
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeUnfold(2).toList == List(1, 2))
  }

  "drop" should "" in {
    assert(Stream(1, 2, 3, 4, 5).drop(2).toList == List(3, 4, 5))
  }

  "takeWhile" should "" in {
    assert(Stream(1, 2, 3, 4, 5).takeWhile((a: Int) => a < 3).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4, 5).takeWhileR((a: Int) => a < 3).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4, 5).takeWhileUnfold((a: Int) => a < 3).toList == List(1, 2))
  }

  "forAll" should "" in {
    assert(Stream(1, 2, 3, 4, 5).forAll((a: Int) => a < 6))
    assert(!Stream(1, 2, 3, 4, 5).forAll((a: Int) => a < 3))
  }

  "headOption" should "" in {
    assert(Stream(1, 2, 3, 4, 5).headOption.contains(1))
    assert(Empty.headOption.isEmpty)
  }

  "map" should "" in {
    assert(Stream(1, 2, 3, 4, 5).map(_ * 10).toList == List(10, 20, 30, 40, 50))
    assert(Stream(1, 2, 3, 4, 5).mapUnfold(_ * 10).toList == List(10, 20, 30, 40, 50))
  }

  "filter" should "" in {
    assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))
  }

  "append" should "" in {
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
  }

  "flatMap" should "" in {
    assert(Stream(1, 2, 3).flatMap(a => Stream(a, a)).toList == List(1, 1, 2, 2, 3, 3))
  }

  "constant" should "" in {
    assert(Stream.constant(5).take(3).toList == List(5, 5, 5))
    assert(Stream.constantUnfold(5).take(3).toList == List(5, 5, 5))
  }

  "from" should "" in {
    assert(Stream.from(5).take(3).toList == List(5, 6, 7))
    assert(Stream.fromUnfold(5).take(3).toList == List(5, 6, 7))
  }

  "fibs" should "" in {
    assert(Stream.fibs.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))
    assert(Stream.fibsUnfold.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  "zipWith" should "" in {
    val l = Stream.fromUnfold(1).zipWith(Stream.fromUnfold(2))((_, _)).take(3).toList
    assert(l == List((1, 2), (2, 3), (3, 4)))
  }

  "zipAll" should "" in {
    val l = Stream.fromUnfold(1).zipAll(Stream(2, 3)).take(3).toList
    assert(l == List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), None)))
    val l2 = Stream(1, 2, 3).zipAll(Stream(2, 3)).take(100).toList
    assert(l2 == List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), None)))
    val l3 = Stream(2, 3).zipAll(Stream(1, 2, 3)).take(100).toList
    assert(l3 == List((Some(2), Some(1)), (Some(3), Some(2)), (None, Some(3))))
  }

  "startsWith" should "" in {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3)))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
  }

  "tails" should "" in {
    assert(Stream(1, 2, 3).tails.toList.map(_.toList) == List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "scanRight" should "" in {
    val l = Stream(1, 2, 3).scanRight(0)(_ + _).toList
    assert(l == List(6, 5, 3, 0))
  }
}
