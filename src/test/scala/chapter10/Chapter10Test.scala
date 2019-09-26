package chapter10

import scala.util.Random

import chapter8._
import org.scalatest.FreeSpec

class Chapter10Test extends FreeSpec {

  private val intGen = Gen.choose(-2000000, 2000000)
  private val booleanGen = Gen.boolean

  "monoid laws" - {
    "hold for int addition" in {
      val prop = Monoid.monoidLaws(Monoid.intAddition, intGen)
      Prop.run(prop)
    }

    "hold for int multiplication" in {
      val prop = Monoid.monoidLaws(Monoid.intMultiplication, intGen)
      Prop.run(prop)
    }

    "hold for boolean or" in {
      val prop = Monoid.monoidLaws(Monoid.booleanOr, booleanGen)
      Prop.run(prop)
    }

    "hold for boolean and" in {
      val prop = Monoid.monoidLaws(Monoid.booleanAnd, booleanGen)
      Prop.run(prop)
    }

    "hold for options" in {
      val prop = Monoid.monoidLaws(
        Monoid.optionMonoid[Int],
        (booleanGen ** booleanGen ** intGen).map { case ((b1, b2), i) => if (!b1 && !b2) None else Some(i) }
      )
      Prop.run(prop)
    }

    "hold for endo" in {
      val prop = Monoid.monoidLaws(
        Monoid.endoMonoid[Int],
        intGen.map(v => (i: Int) => v + i)
      )((a1, a2) => a1(1) == a2(1)) // bit shit
      Prop.run(prop)
    }
  }

  "foldMapV" - {
    "handle empty sequences" in {
      val v = IndexedSeq.empty[String]
      val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
      assert(ans == "")
    }

    "handle a single item" in {
      val v = IndexedSeq("lorem")
      val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
      assert(ans == "lorem")
    }

    "handle even sequences" in {
      val v = IndexedSeq("lorem", "ipsum", "dolor", "sit")
      val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
      assert(ans == "loremipsumdolorsit")
    }

    "handle odd sequences" in {
      val v = IndexedSeq("lorem", "ipsum", "dolor", "sit", "blah")
      val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
      assert(ans == "loremipsumdolorsitblah")
    }
  }

  "sorted int" in {
    assert(Monoid.isSorted(IndexedSeq()) == true)
    assert(Monoid.isSorted(IndexedSeq(1)) == true)
    assert(Monoid.isSorted(IndexedSeq(1, 2, 3, 4)) == true)
    assert(Monoid.isSorted(IndexedSeq(1, 2, 4, 3)) == false)
//    assert(Monoid.isSorted(IndexedSeq(4, 3, 2, 1)) == true)
  }

  "monoid laws" - {
    "hold for WC" in {
      val prop = Monoid.monoidLaws(
        Monoid.wcMonoid,
        for {
          b1 <- booleanGen
          b2 <- booleanGen
          randomStrSeed <- Gen.choose(1, 200000)
          strLength <- Gen.choose(0, 10)
          spaceLocation <- Gen.choose(0, 10)
          numWords <- Gen.choose(0, 4)
        } yield {
          val createStub = b1 && b2
          val r = new Random(randomStrSeed)
          val str = r.nextString(strLength).replaceAll("""\s""", "")

          if (createStub || str.isEmpty) {
            Stub(str)
          } else {
            val safeSpaceLocation = spaceLocation.min(str.length)
            Part(
              str.substring(0, safeSpaceLocation),
              numWords,
              str.substring((safeSpaceLocation + 1).min(str.length))
            )
          }
        }
      )
      Prop.run(prop)
    }
  }

  "countWords" in {
    assert(Monoid.countWords("") == 0)
    assert(Monoid.countWords("a") == 1)
    assert(Monoid.countWords("a b") == 2)
    assert(Monoid.countWords(" a b ") == 2)
    assert(Monoid.countWords(" ab ") == 1)

    val s =
      """When we add up the results of counting the words in these strings, we want to avoid
        |double-counting the word dolor. Clearly, just counting the words as an Int isn't sufficient.
        |We need to find a data structure that can handle partial results like the half words do and
        |lor, and can track the complete words seen so far, like ipsum, sit, and amet.""".stripMargin
    assert(Monoid.countWords(s) == 64)
  }

  "Foldable" - {
    "toList" in {
      val s = (1 to 5).to(LazyList)
      val l = Foldable.lazyList.toList(s)
      assert(l == List(1, 2, 3, 4, 5))
    }
  }

  "monoid laws" - {
    "hold for product" in {
      val prop = Monoid.monoidLaws(
        Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication),
        intGen ** intGen
      )
      Prop.run(prop)
    }
  }

  "bag" in {
    val ans = Monoid.bag(Vector("a", "rose", "is", "a", "rose"))
    assert(ans == Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }
}
