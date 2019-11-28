package chapter15

sealed trait Process[I, O] {
  import Process._

  def apply(s: LazyList[I]): LazyList[O] = this match {
    case Halt() => LazyList()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs) // LazyList is empty
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  // 15.5
  def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
    p2 match {
      case Halt() => Halt()
      case Await(recv2) => this match {
        case Halt() => Halt() |> recv2(None)
        case Await(recv) => Await((i: Option[I]) => recv(i) |> p2)
        case Emit(h, t) => t |> recv2(Some(h))
      }
      case Emit(h2, t2) => Emit(h2, this |> t2)
    }
  }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }
  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }
}

object Process {

  case class Emit[I, O](
    head: O,
    tail: Process[I, O]
  ) extends Process[I, O]

  case class Await[I, O](
    recv: Option[I] => Process[I, O]
  ) extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

  def emit[I, O](
    head: O,
    tail: Process[I, O] = Halt[I, O]()
  ): Process[I, O] = {
    Emit(head, tail)
  }

  def await[I, O](
    f: I => Process[I, O],
    fallback: Process[I, O] = Halt[I, O]()
  ): Process[I, O] = Await[I, O] {
    case Some(i) => f(i)
    case None => fallback
  }

  def liftOne[I,O](f: I => O): Process[I, O] = {
    Await {
      case Some(i) => emit(f(i))
      case None => Halt()
    }
  }

  def lift[I, O](f: I => O): Process[I, O] = {
    liftOne(f).repeat
  }

  def filter[I](f: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if f(i) => emit(i)
      case _ => Halt()
    }.repeat
  }

  // 15.1
  def take[I](n: Int): Process[I, I] = {
    if (n <= 0) Halt()
    else await(i => Emit(i, take(n - 1)))
  }

  def drop[I](n: Int): Process[I, I] = {
    if (n <= 0) await[I, I](emit(_)).repeat
    else await[I, I](_ => drop(n - 1))
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if f(i) => Emit(i, takeWhile(f))
      case _ => Halt()
    }
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if f(i) => dropWhile(f)
      case Some(i) => Emit(i, await[I, I](emit(_)).repeat)
      case None => Halt()
    }
  }

  // 15.2
  def count[I]: Process[I, Int] = {
    def go(c: Int): Process[I, Int] = {
      await[I, Int](_ => Emit(c, go(c + 1)))
    }
    go(1)
  }

  // 15.3
  def mean: Process[Double, Double] = {
    def go(c: Int, s: Double): Process[Double, Double] = {
      await[Double, Double](v => Emit((s + v) / c, go(c + 1, s + v)))
    }
    go(1, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = {
    await((i: I) => f(i, z) match {
      case (o,s2) => emit(o, loop(s2)(f))
    })
  }

  // 15.4
  def sumL: Process[Double, Double] = {
    loop(0.0) { case (v, s) => (s + v, s + v) }
  }

  def countL[I]: Process[I, Int] = {
    loop(1) { case (_, c) => (c, c + 1) }
  }
}
