package chapter13

import scala.annotation.tailrec

import chapter12.MonadApplicative
import chapter7.nonblocking.Par

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {

  // 13.1
  implicit def monad[F[_]]: MonadApplicative[Free[F, *]] = new MonadApplicative[Free[F, *]] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
  }

  // 13.2
  @tailrec def runTrampoline[A](fa: Free[Function0, A]): A = {
    fa match {
      case Return(a) => a
      case Suspend(fa) => fa()
      case FlatMap(fa, f) => fa match {
        case Return(a2) => runTrampoline(f(a2))
        case Suspend(fa2) => runTrampoline(f(fa2()))
        case FlatMap(fa2, g) => runTrampoline(fa2.flatMap(a => g(a) flatMap f))
      }
    }
  }

  // 13.3
  def run[F[_], A](fa: Free[F, A])(implicit F: MonadApplicative[F]): F[A] = {
    step(fa) match {
      case Return(a) => F.unit(a)
      case Suspend(fa) => fa
      case FlatMap(Suspend(fa), f) => F.flatMap(fa)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  @tailrec def step[F[_], A](fa: Free[F, A]): Free[F, A] = fa match {
    case FlatMap(FlatMap(fa2, f), g) => step(fa2.flatMap(a => f(a) flatMap g))
    case FlatMap(Return(a), f) => step(f(a))
    case _ => fa
  }
}

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def toPar: Par[Option[String]] = Par.lazyUnit(run)
  def toThunk: () => Option[String] = () => run

  def run: Option[String] = {
    try Some(scala.io.StdIn.readLine())
    catch { case _: Exception => None }
  }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar: Par[Unit] = Par.lazyUnit(println(line))
  def toThunk: () => Unit = () => println(line)
}

object Console {
  import Translate.~>

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: MonadApplicative[G]): G[A] = {
    Free.step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }

  implicit val function0Monad: MonadApplicative[Function0] = new MonadApplicative[Function0] {
    def unit[A](a: => A): () => A = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]): () => B = () => f(a())()
  }

  implicit val parMonad: MonadApplicative[Par] = new MonadApplicative[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }
  }

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]): () => A = a.toThunk
  }
  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]): Par[A] = a.toPar
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A = {
    runFree[Console, Function0, A](a)(consoleToFunction0)
  }
  def runConsolePar[A](a: Free[Console, A]): Par[A] = {
    runFree[Console, Par, A](a)(consoleToPar)
  }

  // 13.4
  def translate[F[_], G[_], A](free: Free[F, A])(t: F ~> G): Free[G, A] = {
    val tt = new ~>[F, Free[G, *]] {
      override def apply[AA](f: F[AA]): Free[G, AA] = Suspend(t.apply(f))
    }

    runFree(free)(tt)(Free.monad[G])
  }
  def runConsole[A](a: Free[Console, A]): A = {
    Free.runTrampoline(translate(a)(consoleToFunction0))
  }
}

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]
}

// 13.5 Java NIO non-blocking I/O library
