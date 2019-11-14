package chapter14

import org.scalatest.FreeSpec

class Chapter14Test extends FreeSpec {

  "STArray" - {
    "fill" in {
      val runnable = new RunnableST[List[Char]] {
        def apply[S] = for {
          arr <- STArray.apply(5, 'a')
          _ <- arr.fill(Map(1 -> 'b', 4 -> 'c'))
          result <- arr.freeze
        } yield result
      }
      assert(ST.runST(runnable) == List('a', 'b', 'a', 'a', 'c'))
    }
  }

  "quicksort" in {
    assert(Immutable.quicksort(List(3, 2, 4, 5, 1)) == List(1, 2, 3, 4, 5))
  }

  "STHashMap" in {
    val runnable = new RunnableST[(Option[Int], Map[Char, Int])] {
      def apply[S] = for {
        map <- STHashMap.apply('a' -> 1, 'b' -> 2, 'c' -> 3)
        a <- map.get('a')
        _ <- map.update('a', 5)
        result <- map.freeze
      } yield (a, result)
    }
    assert(ST.runST(runnable) == ((Some(1), Map('a' -> 5, 'b' -> 2, 'c' -> 3))))
  }
}
