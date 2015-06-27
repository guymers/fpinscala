import org.scalatest.FlatSpec

class Chapter2Test extends FlatSpec {

  "The Fibonacci sequence" should "be correct" in {
    assert(Chapter2.fib(0) == 0)
    assert(Chapter2.fib(1) == 1)
    assert(Chapter2.fib(2) == 1)
    assert(Chapter2.fib(3) == 2)
    assert(Chapter2.fib(4) == 3)
    assert(Chapter2.fib(5) == 5)
    assert(Chapter2.fib(6) == 8)
    assert(Chapter2.fib(7) == 13)
  }

  "isSorted" should "be correct" in {
    val array = Array(1, 2, 3, 4, 5, 6)
    assert(Chapter2.isSorted[Int](array, _ <= _) == true)
    assert(Chapter2.isSorted[Int](array, _ >= _) == false)
  }
}
