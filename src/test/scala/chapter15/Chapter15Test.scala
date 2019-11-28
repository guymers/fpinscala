package chapter15

import org.scalatest.FreeSpec

class Chapter15Test extends FreeSpec {

  "Process" - {
    "take" in {
      val l = Process.take(3).apply(LazyList(1, 2, 3, 4, 5)).toList
      assert(l == List(1, 2, 3))
    }

    "drop" in {
      val l = Process.drop(3).apply(LazyList(1, 2, 3, 4, 5)).toList
      assert(l == List(4, 5))
    }

    "takeWhile" in {
      val l = Process.takeWhile((i : Int) => i <= 3).apply(LazyList(1, 2, 3, 4, 5, 4, 3, 2, 1)).toList
      assert(l == List(1, 2, 3))
    }

    "dropWhile" in {
      val l = Process.dropWhile((i: Int) => i <= 3).apply(LazyList(1, 2, 3, 4, 5, 4, 3, 2, 1)).toList
      assert(l == List(4, 5, 4, 3, 2, 1))
    }

    "sum" in {
      val l2 = Process.sumL(LazyList(1, 2, 3, 4, 5)).toList
      assert(l2 == List(1, 3, 6, 10, 15))
    }

    "count" in {
      val l = Process.count(LazyList(1, 2, 3, 4, 5, 4, 3, 2, 1)).toList
      assert(l == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
      val l2 = Process.count(LazyList(1, 2, 3, 4, 5, 4, 3, 2, 1)).toList
      assert(l2 == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    "mean" in {
      val l = Process.mean(LazyList(1, 2, 3, 4, 5)).toList
      assert(l == List(1, 1.5, 2, 2.5, 3))
    }

    "|>" in {
      val p = Process.filter((i: Int) => i % 2 == 0) |> Process.lift(_ + 1)
      val l = p(LazyList(1, 2, 3, 4, 5)).toList
      assert(l == List(3, 5))
    }
  }
}
