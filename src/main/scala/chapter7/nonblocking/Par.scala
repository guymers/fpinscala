package chapter7.nonblocking

import java.util.concurrent.Callable
import java.util.concurrent.CountDownLatch
import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

sealed trait Future[+A] {
  private[chapter7] def apply(cb: A => Unit, onError: Throwable => Unit): Unit
}

object Par {

  // 7.10
  def run[A](es: ExecutorService)(p: Par[A]): A = {

    val ref = new AtomicReference[Either[Throwable, A]]
    val latch = new CountDownLatch(1)

    def onError(e: Throwable): Unit = {
      ref.set(Left(e))
      latch.countDown()
    }

    try {
      p(es)(a => {
        ref.set(Right(a))
        latch.countDown()
      }, onError)
    } catch {
      case NonFatal(e) => onError(e)
    }
    latch.await() // Block until the `latch.countDown` is invoked asynchronously
    val r = ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    r match {
      case Left(e) => throw e
      case Right(r) => r
    }
  }

  def unit[A](a: A): Par[A] = _ => new Future[A] {
    def apply(cb: A => Unit, onError: Throwable => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit, onError: Throwable => Unit): Unit = {
      eval(es)(a(es)(cb, onError), onError)
    }
  }

  def eval(es: ExecutorService)(r: => Unit, onError: Throwable => Unit): Unit = {
    es.submit(new Callable[Unit] {
      def call: Unit = {
        try {
          r
        } catch {
          case e: Throwable => onError(e)
        }
      }
    })
    ()
  }

  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = es => new Future[C] {
    def apply(cb: C => Unit, onError: Throwable => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None

      val combiner = Actor[Either[A,B]](es) {
        case Left(a) =>
          if (br.isDefined) eval(es)(cb(f(a, br.get)), onError)
          else ar = Some(a)
        case Right(b) =>
          if (ar.isDefined) eval(es)(cb(f(ar.get, b)), onError)
          else br = Some(b)
      }

      p(es)(a => combiner ! Left(a), onError)
      p2(es)(b => combiner ! Right(b), onError)
    }
  }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit, onError: Throwable => Unit): Unit =
      cb(a)
  }

  /**
   * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
   * This will come in handy in Chapter 13.
   */
  def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit, onError: Throwable => Unit): Unit = f(k)
  }

  // specialized version of `map`
  def map[A,B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit, onError: Throwable => Unit): Unit =
        p(es)(a => eval(es)(cb(f(a)), onError), onError)
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_ :: _)
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = {
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
}
