package chapter12

import chapter10.Foldable
import chapter10.Monoid
import chapter11.Functor
import chapter6.State
import chapter8.Gen
import chapter8.Prop

trait MonadApplicative[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(identity)
  }
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  }
}

object MonadApplicative {
  // 12.5
  def eitherMonad[E]: MonadApplicative[Either[E, *]] = new MonadApplicative[Either[E, *]] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
  }

  val idMonad = new MonadApplicative[Id] {
    override def unit[A](a: => A): A = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def stateMonad[S] = new MonadApplicative[State[S, *]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
  }

  // 12.20
  def composeM[F[_], G[_]](
    F: MonadApplicative[F],
    G: MonadApplicative[G],
    T: Traverse[G]
  ): MonadApplicative[Lambda[x => F[G[x]]]] = {
    new MonadApplicative[Lambda[x => F[G[x]]]] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        F.flatMap(fga) { ga =>
          val fggb = T.traverse(ga)(f)(F)
          F.map(fggb)(G.join)
        }
      }
    }
  }
}

trait Applicative[F[_]] extends Functor[F] { self =>
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
  }
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List.empty[B])) { case (a, fbs) =>
      map2(f(a), fbs)(_ :: _)
    }
  }

  // 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    traverse(fas)(identity)
  }
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((_, _))
  }

  // 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab)((a, f) => f(a))
  }
  def mapApply[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f))(fa)
  }
  def map2Apply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(f.curried))(fa))(fb)
  }

  // 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  // [5]
  def map2Product[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    map(product(fa, fb)) { case (a, b) => f(a, b) }
  }

  // 12.8
  def product[G[_]](G: Applicative[G]): Applicative[Lambda[x => (F[x], G[x])]] = {
    new Applicative[Lambda[x => (F[x], G[x])]] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }
  }

  // 12.9
  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[x => F[G[x]]]] = {
    new Applicative[Lambda[x => F[G[x]]]] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        self.map2(fa, fb) { case (ga, gb) =>
          G.map2(ga, gb)(f)
        }
      }
    }
  }

  // 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldRight(unit(Map.empty[K, V])) { case ((key, fv), map) =>
      map2(map, fv) { case (m, v) => m + (key -> v) }
    }
  }
}

object Applicative {

  val streamApplicative = new Applicative[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
    override def map2[A, B, C](a: LazyList[A], b: LazyList[B])(f: (A, B) => C): LazyList[C] = {
      a.zip(b).map(f.tupled)
    }
  }

  // 12.4
  // given a list of streams extract one element from each and emit as a list, repeat

  // 12.6
  def validationApplicative[E] = new Applicative[Validation[E, *]] {
    import Validation._

    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(_), f @ Failure(_, _)) => f
        case (f @ Failure(_, _), Success(_)) => f
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
      }
    }
  }

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = {
    p match { case (a, (b, c)) => ((a, b), c) }
  }

  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) = {
    (i, i2) => (f(i), g(i2))
  }

  def applicativeLaws[F[_], A, B, C](F: Applicative[F], gen: Gen[F[A]], f: A => B, g: B => C): Prop = {

    val leftIdentity = Prop.forAll(gen) { fa =>
      F.map2(F.unit(()), fa)((_, a) => a) == fa
    }

    val rightIdentity = Prop.forAll(gen) { fa =>
      F.map2(fa, F.unit(()))((a, _) => a) == fa
    }

    val associative = Prop.forAll(gen ** gen ** gen) { case ((fa, fb), fc) =>
      F.product(F.product(fa, fb), fc) == F.map(F.product(fa, F.product(fb, fc)))(assoc)
    }

    val naturality = Prop.forAll(gen ** gen) { case (fa, fb) =>
      val g = f
      F.map2(fa, fb)(productF(f, g)) == F.product(F.map(fa)(f), F.map(fb)(g))
    }

    Functor.functorLaws(F, gen, f, g) && leftIdentity && rightIdentity && associative && naturality
  }

  implicit def monoid[M](M: Monoid[M]): Applicative[Const[M, *]] = {
    new Applicative[Const[M, *]] {
      override def unit[A](a: => A): Const[M, A] = M.zero
      override def map2[A, B, C](m1: Const[M, A], m2: Const[M, B])(f: (A, B) => C): Const[M, C] = M.op(m1, m2)
    }
  }
}


sealed trait Validation[+E, +A] extends Product with Serializable
object Validation {
  final case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]
  final case class Success[A](a: A) extends Validation[Nothing, A]
}


trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  // 12.14
  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(MonadApplicative.idMonad)

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M = {
    traverse[Const[M, *], A, Nothing](as)(f)(Applicative.monoid(mb))
  }

  // 12.15
  // Foldable reduces a value down, map does not change its size

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    traverse[State[S, *], A, B](fa)(f)(MonadApplicative.stateMonad)
  }

  def toListS[A](fa: F[A]): List[A] = {
    traverseS(fa)((a: A) => for {
      as <- State.get[List[A]]
      _  <- State.set(a :: as)
    } yield ()).run(Nil)._2.reverse
  }

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _  <- State.set(s2)
    } yield b).run(s)
  }

  override def toList[A](fa: F[A]): List[A] = {
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
  }

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = {
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
  }

  // 12.16
  def reverse[A](fa: F[A]): F[A] = {
    // ??? ANS:
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
  }

  // 12.17
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(as, z) { (a, b) => ((), f(b, a)) }._2
  }

  def zip[A,B](fa: F[A], fb: F[B]): F[(A, B)] = {
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1
  }

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] = {
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1
  }

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] = {
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1
  }

  // 12.18
  def fuse[G[_], H[_], A, B](
    fa: F[A]
  )(
    f: A => G[B],
    g: A => H[B]
  )(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    traverse[Lambda[x => (G[x], H[x])], A, B](fa) { a =>
      (f(a), g(a))
    }( G product H)
  }

  // 12.19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[Lambda[x => F[G[x]]]] = {
    new Traverse[Lambda[x => F[G[x]]]] {
      override def traverse[H[_], A, B](fga: F[G[A]])(f: A => H[B])(implicit H: Applicative[H]): H[F[G[B]]] = {
        self.traverse(fga) { ga =>
          G.traverse(ga)(f)
        }
      }
    }
  }

}

object Traverse {

  // 12.13
  val traverseList = new Traverse[List] {
    override def traverse[G[_], A, B](la: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      la.foldRight(G.unit(List.empty[B])) { case (a, fbs) =>
        G.map2(f(a), fbs)(_ :: _)
      }
    }
  }
  val traverseOption = new Traverse[Option] {
    override def traverse[G[_], A, B](opt: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = {
      opt match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(Some(_))
      }
    }
  }
  val traverseTree = new Traverse[Tree] {
    override def traverse[G[_], A, B](tree: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      G.map2(f(tree.head), traverseList.traverse(tree.tail)(a => traverse(a)(f))) { case (head, tail) =>
        Tree(head, tail)
      }
    }
  }
}

final case class Tree[+A](head: A, tail: List[Tree[A]])
