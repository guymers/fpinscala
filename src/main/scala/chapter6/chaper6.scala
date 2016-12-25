package chapter6

import scala.annotation.tailrec

  trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  }

  object RNG {

    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    // 6.1
    // ANS: add one to negative numbers
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (nextInt, nextRng) = rng.nextInt
      if (nextInt == Int.MinValue) nonNegativeInt(nextRng)
      else (nextInt, nextRng)
    }

    // 6.2
    // ANS: divide by MaxValue + 1
    def double(rng: RNG): (Double, RNG) = {
      val (nextInt, nextRng) = nonNegativeInt(rng)
      if (nextInt == Int.MaxValue) double(nextRng)
      else (nextInt.toDouble / Int.MaxValue, nextRng)
    }

    // 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (nextInt, nextRng) = rng.nextInt
      val (nextDouble, nextNextRng) = double(nextRng)
      ((nextInt, nextDouble), nextNextRng)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (next, nextRng) = intDouble(rng)
      (next.swap, nextRng)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng1) = double(rng)
      val (d2, rng2) = double(rng1)
      val (d3, rng3) = double(rng2)
      ((d1, d2, d3), rng3)
    }

    // 6.4
    // ANS: use pattern matching
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      (1 to count).foldLeft((List.empty[Int], rng)) { case ((l, r), _) =>
        val (nextInt, nextRng) = r.nextInt
        (l ++ List(nextInt), nextRng)
      }
    }

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    // 6.5
    // ANS: divide by MaxValue + 1
    def doubleMap: Rand[Double] =
      map(nonNegativeInt)(_.toDouble / Int.MaxValue)

    // 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
      }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] =
      both(int, double)

    val randDoubleInt: Rand[(Double, Int)] =
      both(double, int)

    // 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List.empty[A]))((rand, acc) => map2(rand, acc)(_ :: _))
  //      rng => {
  //        fs.foldLeft((List.empty[A], rng)) { case ((l, r), rand) =>
  //          map(rand)(_ :: l)(r)
  //        }
  //      }

    def intsSequence(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    // 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, nextRng) = f(rng)
        g(a)(nextRng)
      }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      @tailrec
      def go(rng: RNG): (Int, RNG) = {
        val (i, rng2) = nonNegativeInt(rng)
        val mod = i % n
        if (i + (n-1) - mod >= 0) (mod, rng2)
        else go(rng2)
      }
      go
    }

    def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod)
        else nonNegativeLessThanWithFlatMap(n)
      }

    // 6.9
    def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra) { a =>
        map(rb) { b =>
          f(a, b)
        }
      }
  }

  // 6.10
  case class State[S, +A](run: S => (A, S)) {
    import State._

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
      a <- this
      b <- sb
    } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, newS) = run(s)
      f(a).run(newS)
    }
  }

  object State {

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    // ANS: foldRight to preserve order
    def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
      ls.foldRight(unit[S, List[A]](List.empty))((rand, acc) => rand.map2(acc)(_ :: _))
  //    State { s =>
  //      ls.foldLeft((List.empty[A], s)) { case ((list, prevState), state) =>
  //        state.map(_ :: list).run(prevState)
  //      }
  //    }
  }

  // 6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {

    /**
      * The rules of the machine are as follows:
      *
      *   Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
      *   Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
      *   Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
      *   A machine that’s out of candy ignores all inputs.
      *
      * @param input
      * @return
      */
    def apply(input: Input): Machine = {
      if (candies <= 0) {
        this
      } else {
        input match {
          case Coin => if (locked) coinAdded else this
          case Turn => if (!locked) candyRemoved else this
        }
      }
    }

    private def coinAdded = copy(locked = false, coins = coins + 1)
    private def candyRemoved = copy(locked = true, candies = candies - 1)
  }

  object Machine {
    import State._

    val buyCandy = List(Coin, Turn)

  //    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine =>
  //      val t = inputs.foldLeft(machine)((m, input) => m.apply(input))
  //
  //      ((t.coins, t.candies), t)
  //    }
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs.map { input =>
        modify[Machine](_ apply input)
      })
      machine <- get
    } yield (machine.coins, machine.candies)
  }
