package chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, nextRNG) = ra(rng)
    (f(a), nextRNG)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRNG) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nextRNG) // MinValue != MaxValue
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRNG) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRNG1) = rng.nextInt
    val (d, nextRNG2) = double(nextRNG1)
    ((i, d), nextRNG2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, nextRNG1) = double(rng)
    val (i, nextRNG2) = nextRNG1.nextInt
    ((d, i), nextRNG2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nextRNG1) = double(rng)
    val (d2, nextRNG2) = double(nextRNG1)
    val (d3, nextRNG3) = double(nextRNG2)
    ((d1, d2, d3), nextRNG3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i, nextRNG1) = rng.nextInt
      val (ls, nextRNG2) = ints(count - 1)(nextRNG1)
      (i :: ls, nextRNG2)
    }
  }

  // 6.5
  val doubleMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, nextRNG1) = ra(rng)
    val (b, nextRNG2) = rb(nextRNG1)
    (f(a, b), nextRNG2)
  }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A])) { case (ra, rla) =>
      map2(ra, rla)(_ :: _)
    }
  }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  // 6.8
  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, nextRNG) = ra(rng)
    f(a)(nextRNG)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    @tailrec def go(rng: RNG): (Int, RNG) = {
      val (i, nextRNG) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, nextRNG)
      else go(nextRNG)
    }
    go
  }

  def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThanWithFlatMap(n)
    }
  }

  // 6.9
  def mapFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] = {
    flatMap(ra)(a => unit(f(a)))
  }

  def map2FlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }
}

// 6.10
object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight(unit[S, List[A]](Nil)) { case (sa, sla) =>
      sa.map2(sla)(_ :: _)
    }
  }
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s_) = run(s)
    (f(a), s_)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s_) = run(s)
    f(a).run(s_)
  }
}


// 6.11
object Machine {

  sealed trait Input
  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  val buyCandy: List[Input] = List(Input.Coin, Input.Turn)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(handleInput))
    machine <- State.get
  } yield (machine.coins, machine.candies)

  private def handleInput(input: Input): State[Machine, Unit] = State { machine =>
    ((), machine.handleInput(input))
  }
}

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def handleInput(input: Machine.Input): Machine = {
    if (candies <= 0) this
    else input match {
      case Machine.Input.Coin => handleCoin
      case Machine.Input.Turn => handleTurn
    }
  }

  def handleCoin: Machine = {
    if (locked && candies > 0) copy(locked = false, coins = coins + 1)
    else this
  }

  def handleTurn: Machine = {
    if (!locked && candies > 0) copy(locked = true, candies = candies - 1)
    else this
  }

}
