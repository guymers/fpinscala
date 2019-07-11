package chapter3

import org.scalatest.FreeSpec

class Chapter3Test extends FreeSpec {

  import List._

  "match" in {
    val result = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, _) => h + 1000 // sum(t)
      case _ => 101
    }

    assert(result == 3)
  }

  "tail" in {
    val l = List(1, 2, 3, 4, 5)
    assert(tail(l) == List(2, 3, 4, 5))
    assert(tail(Nil) == Nil)
  }

  "setHead" in {
    val l = List(1, 2, 3, 4, 5)
    assert(setHead(l, 9) == List(9, 2, 3, 4, 5))
    assert(setHead(Nil, 9) == Nil)
  }

  "drop" in {
    val l = List(1, 2, 3, 4, 5)
    assert(drop(l, 0) == List(1, 2, 3, 4, 5))
    assert(drop(l, 1) == List(2, 3, 4, 5))
    assert(drop(l, 3) == List(4, 5))
    assert(drop(Nil, 1) == Nil)
  }

  "dropWhile" in {
    val l = List(1, 2, 3, 4, 5)
    assert(dropWhile[Int](l, _ < 4) == List(4, 5))
    assert(dropWhile[Int](Nil, _ < 4) == Nil)
  }

  "init" in {
    val l = List(1, 2, 3, 4)
    assert(init(l) == List(1, 2, 3))
    assert(init(Nil) == Nil)
  }

  "length" in {
    val l = List(1, 2, 3, 4)
    assert(length(l) == 4)
    assert(length(Nil) == 0)
  }

  "foldLeft" in {
    val result = foldLeft(List(1, 2, 3), List.empty[Int]) { case (t, h) => Cons(h, t) }
    assert(result == List(3, 2, 1))
  }

  "foldLeft functions" in {
    val l = List(1, 2, 3, 4)
    assert(sumL(l) == 10)
    assert(productL(List(1.0, 2.0, 3.0, 4.0)) == 24.0)
    assert(lengthL(l) == 4)
  }

  "reverse" in {
    val l = List(1, 2, 3)
    assert(reverse(l) == List(3, 2, 1))
  }

  "foldLeft in terms of foldRight" in {
    val result1 = foldLeft(List(1, 2, 3), List.empty[Int]) { case (t, h) => Cons(h, t) }
    assert(result1 == List(3, 2, 1))
    val result2 = foldLeftRight(List(1, 2, 3), List.empty[Int]) { case (t, h) => Cons(h, t) }
    assert(result2 == List(3, 2, 1))
  }

  "foldRight in terms of foldLeft" in {
    val result1 = foldRight(List(1, 2, 3), List.empty[Int])(Cons(_, _))
    assert(result1 == List(1, 2, 3))
    val result2 = foldRightLeft(List(1, 2, 3), List.empty[Int])(Cons(_, _))
    assert(result2 == List(1, 2, 3))
  }

  "append" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    assert(append(l1, l2) == List(1, 2, 3, 4, 5, 6))
  }

  "concat" in {
    val l = List(List(1, 2), List(3, 4), List(5, 6))
    assert(concat(l) == List(1, 2, 3, 4, 5, 6))
  }

  "intPlus1" in {
    val l = List(1, 2, 3)
    assert(intPlus1(l) == List(2, 3, 4))
  }

  "doubleToString" in {
    val l = List(1.0, 1.5, 2.0)
    assert(doubleToString(l) == List("1.0", "1.5", "2.0"))
  }

  "map" in {
    val l = List(1, 2, 3)
    assert(map(l)(_ + 5) == List(6, 7, 8))
  }

  "filter" in {
    val l = List(1, 2, 3)
    assert(filter(l)(_ % 2 == 0) == List(2))
    assert(filterFlatMap(l)(_ % 2 == 0) == List(2))
  }

  "flatMap" in {
    val l = List(1, 2, 3)
    assert(flatMap(l)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  "zipPlus" in {
    assert(zipPlus(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(zipPlus(List(1, 2, 3, 4), List(4, 5, 6)) == List(5, 7, 9))
    assert(zipPlus(List(1, 2, 3), List(4, 5, 6, 7)) == List(5, 7, 9))
  }

  "zipWith" in {
    assert(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
    assert(zipWith(List(1, 2, 3, 4), List(4, 5, 6))(_ + _) == List(5, 7, 9))
    assert(zipWith(List(1, 2, 3), List(4, 5, 6, 7))(_ + _) == List(5, 7, 9))
  }

  "hasSubsequence" in {
    val l = List(1, 2, 3, 4)
    assert(hasSubsequence(l, List(1, 2)))
    assert(hasSubsequence(l, List(2, 3)))
    assert(hasSubsequence(l, List(4)))
    assert(!hasSubsequence(l, List(2, 4)))
  }

  "tree size" in {
    val t = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    assert(Tree.size(t) == 7)
    assert(Tree.sizeFold(t) == 7)
  }

  "tree maximum" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(9))), Branch(Leaf(3), Leaf(7)))
    assert(Tree.maximum(t) == 9)
    assert(Tree.maximumFold(t) == 9)
  }

  "tree depth" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(9))), Branch(Leaf(3), Leaf(7)))
    assert(Tree.depth(t) == 3)
    assert(Tree.depthFold(t) == 3)

    val t2 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    assert(Tree.depth(t2) == 2)
    assert(Tree.depthFold(t2) == 2)
  }

  "tree map" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(9))), Branch(Leaf(3), Leaf(7)))
    assert(Tree.map(t)(_ + 1) == Branch(Branch(Leaf(2), Branch(Leaf(6), Leaf(10))), Branch(Leaf(4), Leaf(8))))
    assert(Tree.mapFold(t)(_ + 1) == Branch(Branch(Leaf(2), Branch(Leaf(6), Leaf(10))), Branch(Leaf(4), Leaf(8))))
  }
}
