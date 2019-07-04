package chapter2

import org.scalatest.FreeSpec

class Chapter2Test extends FreeSpec {

  "fib" in {
    def test(f: Int => Long) = {
      assert(f(0) == 0)
      assert(f(1) == 1)
      assert(f(2) == 1)
      assert(f(3) == 2)
      assert(f(4) == 3)
      assert(f(5) == 5)
      assert(f(6) == 8)
      assert(f(7) == 13)
      assert(f(8) == 21)
      assert(f(9) == 34)
      assert(f(90) == 2880067194370816120L)
    }
    test(fib)
    test(fib2)
  }

  "isSorted" in {
    def asc(n1: Int, n2: Int): Boolean = n1 <= n2

    assert(isSorted(Array(1, 2, 3, 4, 5), asc))
    assert(isSorted(Array(1, 1, 1, 1, 1), asc))
    assert(!isSorted(Array(1, 0, 1, 1, 1), asc))
  }
}
