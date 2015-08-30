package chapter7

import java.util.concurrent._

import scala.concurrent.duration.Duration

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // 7.3
  def map2Timeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(timeout: Duration): Par[C] = es => {
    val start = System.nanoTime()
    val af = a(es)
    val bf = b(es)
    val ar = af.get(timeout.toNanos, TimeUnit.NANOSECONDS)
    val end = System.nanoTime()
    val br = bf.get(timeout.toNanos - (end - start), TimeUnit.NANOSECONDS)
    UnitFuture(f(ar, br))
  }

  // 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A])) { (a, l) => map2(a, l)(_ :: _) }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight(unit(List.empty[A])) { (a, l) =>
      map2(asyncF(f)(a), l) { (filter, ls) => if (filter) a :: ls else ls }
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  // 7.7
  // given map(y)(id) == y, then map(map(y)(g))(f) == map(y)(f compose g)
  //
  // map(map(y)(g))(f) == map(y)(f(g))
  // map(map(y)(id))(f) == map(y)(f(id)) // substitute g for id
  // map(y)(f) == map(y)(f(id)) // substitute map(y)(id) == y
  // map(y)(f) == map(y)(f) // substitute f(id) == f

  // 7.8
  // the Executors.newSingleThreadExecutor* break the law as per the comment

  // 7.9
  // if multiple forks submit callables at the same time filling the pool
  // before each one blocks waiting for the future

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val nn = run(es)(n).get
    run(es)(choices(nn))
  }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 1 else 0))(List(f, t))

  // 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => {
    val k = run(es)(key).get
    run(es)(choices.get(k).get)
  }

  // 7.13
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get
    choices(a)(es)
  }

  def choiceC[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(a => if (a) t else f)

  def choiceNC[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(a => choices.drop(a).head)

  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] =
    //es => a(es).get.apply(es)
    es => run(es)(run(es)(a).get())

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))
}
