package chapter14

import scala.collection.mutable
import scala.reflect.ClassTag

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def noop[S]: ST[S, Unit] = ST[S, Unit](())

  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A = {
    st[Unit].run(())._1
  }
}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST {
    new STRef[S, A] {
      var cell: A = a
    }
  }
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A] {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  // 14.1
  def fill(xs: Map[Int, A]): ST[S, Unit] = {
    xs.foldLeft(ST.noop[S]) { case (st, (i, a)) =>
      st.flatMap(_ => write(i, a))
    }
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A : ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] = ST {
    new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    }
  }

  def fromList[S, A : ClassTag](xs: List[A]): ST[S, STArray[S, A]] = ST {
    new STArray[S, A] {
      lazy val value: Array[A] = xs.toArray
    }
  }

}

object Immutable {

  // 14.2
  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    pivotVal <- a.read(pivot)
    _ <- a.swap(pivot, r)
    jRef <- STRef(l)
    _ <- (l until r).foldLeft(ST.noop[S]) { case (st, i) =>
      for {
        _ <- st
        c <- a.read(i)
        _ <- if (c < pivotVal) for {
          j <- jRef.read
          _ <- a.swap(i, j)
          _ <- jRef.write(j + 1)
        } yield () else ST.noop[S]
      } yield ()
    }
    j <- jRef.read
    _ <- a.swap(j, r)
  } yield j

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield () else ST.noop[S]

  def quicksort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })
  }
}

// 14.3
sealed abstract class STHashMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(value.size)

  def get(k: K): ST[S, Option[V]] = ST(value.get(k))

  def update(k: K, v: V): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value.update(k, v)
      ((), s)
    }
  }

  def freeze: ST[S, Map[K, V]] = ST(value.toMap)

}

object STHashMap {

  def apply[S, K, V](elems: (K, V)*): ST[S, STHashMap[S, K, V]] = ST {
    new STHashMap[S, K, V] {
      lazy val value: mutable.HashMap[K, V] = mutable.HashMap.apply(elems: _*)
    }
  }

  def fromMap[S, K, V](map: Map[K, V]): ST[S, STHashMap[S, K, V]] = ST {
    new STHashMap[S, K, V] {
      lazy val value: mutable.HashMap[K, V] = mutable.HashMap.from(map)
    }
  }

}
