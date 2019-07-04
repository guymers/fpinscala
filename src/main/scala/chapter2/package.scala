import scala.annotation.tailrec

package object chapter2 {

  // 2.1
  def fib(n: Int): Long = {
    @tailrec
    def go(num: Int, prev: Long, cur: Long): Long = {
      if (num >= n) cur
      else go(num + 1, cur, cur + prev)
    }
    if (n <= 0) 0 else go(1, 0, 1)
  }

  def fib2(n: Int): Long = {
    @tailrec
    def go(c: Int, p1: Long, p2: Long): Long = c match {
      case c if c > n => p1 + p2
      case 1 => go(c + 1, 1, 0)
      case 2 => go(c + 1, 1, 0)
      case c => go(c + 1, p1 + p2, p1)
    }
    go(1, 0, 0)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(as: Array[A]): Boolean = {
      if (as.length <= 1) true
      else ordered(as(0), as(1)) && go(as.tail)
    }
    go(as)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
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
