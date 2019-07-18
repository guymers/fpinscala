package object chapter4 {

  sealed trait Option[+A] {
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(Some(_)).getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = {
      if (map(f).getOrElse(false)) this else None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {

    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    // 4.2
    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).map { m =>
        xs.map(x => math.pow(x - m, 2))
      }.flatMap(mean)
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    // 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a.flatMap { a =>
        b.map(b => f(a, b))
      }
    }

    // 4.4
    def sequence[A](l: List[Option[A]]): Option[List[A]] = {
      val empty: Option[List[A]] = Some(Nil)
      l.foldRight(empty) { case (a, ls) =>
        map2(a, ls)(_ :: _)
      }
    }

    def sequencePatternMatch[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case x :: xs => x.flatMap(x => sequence(xs).map(x :: _))
    }

    // 4.5
    def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
      val empty: Option[List[B]] = Some(Nil)
      l.foldRight(empty) { case (a, ls) =>
        map2(f(a), ls)(_ :: _)
      }
    }

    def traversePatternMatch[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case x :: xs => f(x).flatMap(x => traverse(xs)(f).map(x :: _))
    }

    def sequenceT[A](l: List[Option[A]]): Option[List[A]] = traverse(l)(identity)
  }


  sealed trait Either[+E, +A] {
    // 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case l @ Left(_) => l
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case l @ Left(_) => l
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case r @ Right(_) => r
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
  case class Left[+E](get: E) extends Either[E, Nothing]
  case class Right[+A](get: A) extends Either[Nothing, A]

  object Either {
    // 4.7
    def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] = {
      val empty: Either[E, List[A]] = Right(Nil)
      l.foldRight(empty) { case (a, ls) =>
        a.map2(ls)(_ :: _)
      }
    }

    def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      val empty: Either[E, List[B]] = Right(Nil)
      l.foldRight(empty) { case (a, ls) =>
        f(a).map2(ls)(_ :: _)
      }
    }
  }

}
