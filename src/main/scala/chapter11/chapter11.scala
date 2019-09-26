package chapter11

import chapter6.State
import chapter7.Par
import chapter8.Gen
import chapter8.Prop
import chapter9.Parsers

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {

  def functorLaws[F[_], A, B, C](F: Functor[F], gen: Gen[F[A]], f: A => B, g: B => C): Prop = {
    val identity = Prop.forAll(gen) { fa =>
      F.map(fa)(a => a) == fa
    }

    val composition = Prop.forAll(gen) { fa =>
      F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
    }

    identity && composition
  }

  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }
}


trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  }
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((_, _))
  }

  // 11.3
  def sequence[A](lfa: List[F[A]]): F[List[A]] = {
    traverse(lfa)(identity)
  }
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List.empty[B])) { case (a, flb) =>
      map2(f(a), flb)(_ :: _)
    }
  }

  // 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  // 11.6
  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = {
    la.foldRight(unit(List.empty[A])) { case (a, fla) =>
      map2(f(a), fla) { case (b, ls) =>
        if (b) a :: ls else ls
      }
    }
  }

  // 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }

  // 11.8
  def flatMapCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    compose((_: Unit) => fa, f)(())
  }

  // 11.9
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
  // compose(compose(f, g), h) == compose(f, compose(g, h))

  // 11.11
  // flatMap(x)(unit) == x
  //   flatMap(None)(unit) == None
  //   None == None
  // flatMap(x)(unit) == x
  //   flatMap(Some(5))(unit) == Some(5)
  //   unit(5) == Some(5)

  // 11.12
  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(identity)
  }

  // 11.13
  def flatMapJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    join(map(fa)(f))
  }

  // 11.14
  // join(map(x)(unit)) == x
  // join(map(unit(y))(f)) == f(y)
  // join(map(join(map(x)(f)))(g)) == join(map(x)(a => join(map(f(a))(g))))
}

object Monad {

  import chapter5.Stream

  def monadLaws[F[_], A, B, C](F: Monad[F], gen: Gen[F[A]], f: A => B, g: B => C): Prop = {
    val _f = (a: A) => F.unit(f(a))
    val _g = (b: B) => F.unit(g(b))

//    val leftIdentity = Prop.forAll(gen) { a =>
//      F.flatMap(F.unit(a))(_f) == _f(a)
//    }

    val rightIdentity = Prop.forAll(gen) { fa =>
      F.flatMap(fa)(F.unit(_)) == fa
    }

    val associative = Prop.forAll(gen) { fa =>
      F.flatMap(F.flatMap(fa)(_f))(_g) == F.flatMap(fa)(a => F.flatMap(_f(a))(_g))
    }

    Functor.functorLaws(F, gen, f, g) && rightIdentity && associative
  }

  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  // 11.1
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    override def unit[A](a: => A) = p.succeed(a)
    override def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  // 11.2
  def stateMonad[S] = new Monad[State[S, *]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
  }
}

// 11.17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id extends Monad[Id] {
  override def unit[A](a: => A): Id[A] = Id(a)
  override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f
}

// 11.20
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[Reader[R, *]] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      Reader(r => f(st.run(r)).run(r))
    }
  }
}
