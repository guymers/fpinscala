import scala.annotation.tailrec

object Chapter2 {

  // 2.1 - 0, 1, 1, 2, 3, 5...
  def fib(n: Int): Int = {
    @tailrec
    def go(num: Int, prev: Int, cur: Int): Int = {
      if (num >= n) cur
      else go(num + 1, cur, cur + prev)
    }
    if (n <= 0) 0 else go(1, 0, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(a: Array[A], acc: Boolean): Boolean = {
      if (a.length <= 1) acc
      else go(a.tail, acc && ordered(as(0), as(1)))
    }
    go(as, true)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
