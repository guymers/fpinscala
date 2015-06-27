import org.scalatest.FlatSpec

class Chapter3Test extends FlatSpec {

  import Chapter3._
  import List._

  "3.1" should "be 3" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    assert(x == 3)
  }

  "tail" should "" in {
    val l = List(1, 2, 3, 4, 5)
    assert(tail(l) == List(2, 3, 4, 5))
  }

  "setHead" should "" in {
    val l = List(1, 2, 3, 4, 5)
    assert(setHead(l, 9) == List(9, 2, 3, 4, 5))
  }

  "drop" should "" in {
    val l = List(1, 2, 3, 4, 5)
    assert(drop(l, 0) == List(1, 2, 3, 4, 5))
    assert(drop(l, 1) == List(2, 3, 4, 5))
    assert(drop(l, 3) == List(4, 5))
  }

  "dropWhile" should "" in {
    val l = List(1, 2, 3, 4, 5)
    assert(dropWhile[Int](l, _ < 4) == List(4, 5))
  }

  "init" should "" in {
    val l = List(1, 2, 3, 4)
    assert(init(l) == List(1, 2, 3))
  }

  "foldRight" should "" in {
    val result = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    assert(result == List(1, 2, 3))

    val resultL = foldRightL(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    assert(resultL == List(1, 2, 3))
  }

  "length" should "" in {
    val l = List(1, 2, 3, 4)
    assert(length(l) == 4)
  }

  "foldLeft" should "" in {
    val result = foldLeft(List(1, 2, 3), Nil: List[Int])((ll, head) => Cons(head, ll))
    assert(result == List(3, 2, 1))

    val resultR = foldLeftR(List(1, 2, 3), Nil: List[Int])((ll, head) => Cons(head, ll))
    assert(resultR == List(3, 2, 1))
  }

  "foldLeft functions" should "" in {
    val l = List(1, 2, 3, 4)
    assert(suml(l) == 10)
    assert(productl(List(1.0, 2.0, 3.0, 4.0)) == 24.0)
    assert(lengthl(l) == 4)
  }

  "reverse" should "" in {
    val l = List(1, 2, 3)
    assert(reverse(l) == List(3, 2, 1))
  }

  "append" should "" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    assert(append(l1, l2) == List(1, 2, 3, 4, 5, 6))
  }

  "concat" should "" in {
    val l = List(List(1, 2), List(3, 4), List(5, 6))
    assert(concat(l) == List(1, 2, 3, 4, 5, 6))
  }

  "intPlus1" should "" in {
    val l = List(1, 2, 3)
    assert(intPlus1(l) == List(2, 3, 4))
  }

  "doubleToString" should "" in {
    val l = List(1.0, 1.5, 2.0)
    assert(doubleToString(l) == List("1.0", "1.5", "2.0"))
  }

  "map" should "" in {
    val l = List(1, 2, 3)
    assert(map(l)(_ + 5) == List(6, 7, 8))
  }

  "filter" should "" in {
    val l = List(1, 2, 3)
    assert(filter(l)(_ % 2 == 0) == List(2))
    assert(filterFM(l)(_ % 2 == 0) == List(2))
  }

  "flatMap" should "" in {
    val l = List(1, 2, 3)
    assert(flatMap(l)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  "zipWith" should "" in {
    assert(zipWith[Int, Int, Int](List(1, 2, 3), List(4, 5, 6), _ + _) == List(5, 7, 9))
    assert(zipWith[Int, Int, Int](List(1, 2, 3, 4), List(4, 5, 6), _ + _) == List(5, 7, 9))
    assert(zipWith[Int, Int, Int](List(1, 2, 3), List(4, 5, 6, 7), _ + _) == List(5, 7, 9))
  }

  "hasSubsequence" should "" in {
    val l = List(1, 2, 3, 4)
    assert(hasSubsequence(l, List(1, 2)))
    assert(hasSubsequence(l, List(2, 3)))
    assert(hasSubsequence(l, List(4)))
    assert(!hasSubsequence(l, List(2, 4)))
  }

  "tree size" should "" in {
    val t = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    assert(Tree.size(t) == 7)
    assert(Tree.sizeF(t) == 7)
  }

  "tree maximum" should "" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(9))), Branch(Leaf(3), Leaf(7)))
    assert(Tree.maximum(t) == 9)
    assert(Tree.maximumF(t) == 9)
  }

  "tree depth" should "" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(9))), Branch(Leaf(3), Leaf(7)))
    assert(Tree.depth(t) == 3)
    assert(Tree.depthF(t) == 3)
    val t2 = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    assert(Tree.depth(t2) == 2)
    assert(Tree.depthF(t2) == 2)
  }

  "tree map" should "" in {
    val t = Branch(Branch(Leaf(1), Branch(Leaf(5), Leaf(9))), Branch(Leaf(3), Leaf(7)))
    assert(Tree.map(t)(_ + 1) == Branch(Branch(Leaf(2), Branch(Leaf(6), Leaf(10))), Branch(Leaf(4), Leaf(8))))
  }
}
