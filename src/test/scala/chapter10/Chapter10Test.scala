package chapter10

import chapter6.RNG
import chapter8._
import org.scalatest.{EitherValues, FlatSpec}

import scala.util.Random

class Chapter10Test extends FlatSpec with EitherValues {

  private val intGen = Gen.choose(-2000000, 2000000)
  private val booleanGen = Gen.boolean

  "monoid laws" should "hold for int addition" in {
    val prop = Monoid.monoidLaws(Monoid.intAddition, intGen)
    run(prop)
  }

  "monoid laws" should "hold for int multiplication" in {
    val prop = Monoid.monoidLaws(Monoid.intMultiplication, intGen)
    run(prop)
  }

  "monoid laws" should "hold for boolean or" in {
    val prop = Monoid.monoidLaws(Monoid.booleanOr, booleanGen)
    run(prop)
  }

  "monoid laws" should "hold for boolean and" in {
    val prop = Monoid.monoidLaws(Monoid.booleanAnd, booleanGen)
    run(prop)
  }

  "monoid laws" should "hold for options" in {
    val prop = Monoid.monoidLaws(
      Monoid.optionMonoid[Int],
      (booleanGen ** booleanGen ** intGen).map(v => if (!v._1._1 && !v._1._2) None else Some(v._2))
    )
    run(prop)
  }

//  "monoid laws" should "hold for endo" in {
//    // TODO function equality?
//    val prop = Monoid.monoidLaws(
//      Monoid.endoMonoid[Int],
//      intGen.map(v => (i: Int) => v + i)
//    )
//    run(prop)
//  }

  "foldMapV" should "handle empty sequences" in {
    val v = IndexedSeq.empty[String]
    val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
    assert(ans === "")
  }

  "foldMapV" should "handle a single item" in {
    val v = IndexedSeq("lorem")
    val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
    assert(ans === "lorem")
  }

  "foldMapV" should "handle even sequences" in {
    val v = IndexedSeq("lorem", "ipsum", "dolor", "sit")
    val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
    assert(ans === "loremipsumdolorsit")
  }

  "foldMapV" should "handle odd sequences" in {
    val v = IndexedSeq("lorem", "ipsum", "dolor", "sit", "blah")
    val ans = Monoid.foldMapV(v, Monoid.stringConcatenation)(identity)
    assert(ans === "loremipsumdolorsitblah")
  }

  "sorted int" should "" in {
    assert(Monoid.isSorted(IndexedSeq()) === true)
    assert(Monoid.isSorted(IndexedSeq(1)) === true)
    assert(Monoid.isSorted(IndexedSeq(1, 2, 3, 4)) === true)
    assert(Monoid.isSorted(IndexedSeq(1, 2, 4, 3)) === false)
//    assert(Monoid.isSorted(IndexedSeq(4, 3, 2, 1)) === true)
  }

  "monoid laws" should "hold for WC" in {
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
    run(prop)
  }

  "create WC" should "" in {
    assert(Monoid.toWC("lorem ipsum do") === Part("lorem", 1, "do"))
    assert(Monoid.toWC("lor sit amet, ") === Part("lor", 2, ""))
    assert(Monoid.toWC("we") === Stub("we"))
    assert(Monoid.toWC(" i") === Part("", 0, "i"))
    assert(Monoid.toWC("i ") === Part("i", 0, ""))
    assert(Monoid.toWC("") === Stub(""))
    assert(Monoid.toWC(" ") === Stub(" "))
  }

  "countWords" should "" in {
    val s =
      """When we add up the results of counting the words in these strings, we want to avoid
        |double-counting the word dolor. Clearly, just counting the words as an Int isn’t sufficient.
        |We need to find a data structure that can handle partial results like the half words do and
        |lor, and can track the complete words seen so far, like ipsum, sit, and amet.""".stripMargin
    val ans = Monoid.countWords(s)
    assert(ans === 62)
  }

  "Foldable" should "toList" in {
    val s = (1 to 5).toStream
    val l = Foldable.stream.toList(s)
    assert(l == List(1, 2, 3, 4, 5))
  }

  "monoid laws" should "hold for product" in {
    val prop = Monoid.monoidLaws(
      Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication),
      intGen ** intGen
    )
    run(prop)
  }

  "bag" should "" in {
    val ans = Monoid.bag(Vector("a", "rose", "is", "a", "rose"))
    assert(ans === Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }

  private def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis())
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => fail(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
}
