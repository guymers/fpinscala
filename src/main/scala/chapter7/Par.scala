package chapter7

import java.util.concurrent._

import scala.concurrent.duration.Duration

object Par {

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled = false
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(() => a(es).get)
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // 7.3
  def map2Timeout[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C)(timeout: Duration): Par[C] = es => {
    val start = System.nanoTime()
    val af = a(es)
    val bf = b(es)
    val ar = af.get(timeout.toNanos, TimeUnit.NANOSECONDS)
    val end = System.nanoTime()
    val br = bf.get(timeout.toNanos - (end - start), TimeUnit.NANOSECONDS)
    UnitFuture(f(ar, br))
  }

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List.empty[A])) { case (pa, pla) =>
      map2(pa, pla)(_ :: _)
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val seq = sequence(as.map(asyncF(a => Some(a).filter(f))))
    map(seq)(_.flatten)
  }

  // 7.7
  // given:  map(map(y)(g))(f) == map(y)(f compose g)
  // let g = id:  map(map(y)(id))(f) == map(y)(f compose id)
  // substitute map(y)(id) == y:  map(y)(f) == map(y)(f compose id)
  // f compose id = f:  QED

  // 7.8
  // the Executors.newSingleThreadExecutor* break the law as per the comment

  // 7.9
  // if multiple forks submit callables at the same time filling the pool
  // before each one blocks waiting for the future

  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val i = run(es)(n).get()
    val choice = choices.drop(i).headOption.getOrElse(choices.last)
    choice(es)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(b => if (b) 1 else 0))(List(f, t))
  }

  // 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val k = run(es)(key).get()
    val choice = choices.apply(k)
    choice(es)
  }

  // 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get()
    choices(a)(es)
  }

  def choiceChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser(cond)(b => if (b) t else f)
  }

  def choiceNChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(i => choices.drop(i).headOption.getOrElse(choices.last))
  }

  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] = es => {
    a(es).get()(es)
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

}
