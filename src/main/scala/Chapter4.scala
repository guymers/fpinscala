object Chapter4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(v) => f(v)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case s @ Some(_) => s
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case s @ Some(v) if f(v) => s
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).map(m => xs.map(x => math.pow(x - m, 2))).flatMap(mean)

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case x :: xs => x.flatMap(x => sequence(xs).map(x :: _))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case x :: xs => f(x).flatMap(x => traverse(xs)(f).map(x :: _))
    }
  }


  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case l @ Left(_) => l
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case l @ Left(_) => l
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case r @ Right(_) => r
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      flatMap(a => b.map(f(a, _)))
  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {
    def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
      case Nil => Right(Nil)
      case x :: xs => f(x).flatMap(x => traverse(xs)(f).map(x :: _))
    }

    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
      case Nil => Right(Nil)
      case x :: xs => x.flatMap(x => sequence(xs).map(x :: _))
    }

    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty) Left("mean of empty list!")
      else Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

  }
}
