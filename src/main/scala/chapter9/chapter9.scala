package chapter9

import chapter8.{Gen, Prop}

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>

  val digits: Parser[Int] = map(regex("[0-9]+".r))(_.toInt)

  def char(c: Char): Parser[Char] =
    map(string(c.toString))(_.charAt(0))
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  def succeed[A](a: A): Parser[A] =
    map(string(""))(_ => a) //string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // 9.1
  def map2_1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2)) { case (a, b) => f(a, b) }

  // 9.3
  private def prepend[A](p1: Parser[A], p2: => Parser[List[A]]): Parser[List[A]] = {
    map2(p1, p2) { case (a, b) => a :: b }
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(prepend(p, many(p)), succeed(Nil))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    prepend(p, many(p))

  // 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @tailrec
    def go(l: Parser[List[A]], count: Int): Parser[List[A]] =
      if (count <= 0) l
      else go(prepend(p, l), count - 1)
    go(succeed(Nil), n).map(_.reverse)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // 9.6
  def numChars[A](p: Parser[A]): Parser[List[A]] =
    flatMap(digits)(n => listOfN(n, p))

  // 9.7
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p) { pA =>
      flatMap(p2)(pB => succeed((pA, pB)))
    }

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p) { pA =>
      flatMap(p2)(pB => succeed(f(pA, pB)))
    }

  // 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B >: A](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B >: A](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

// 9.9
object JsonParser {

  type ParseError = String
  case class ParseSuccess[+A](value: A, strValue: String, remainingInput: String)
  type ParseResult[+A] = Either[ParseError, ParseSuccess[A]]

  trait JsonParser[+A] {
    def parse(input: String): ParseResult[A]
  }

  val parsers = new Parsers[ParseError, JsonParser] {
    override implicit def string(s: String): JsonParser[String] = new JsonParser[String] {
      override def parse(input: String): ParseResult[String] = {
        if (input.startsWith(s)) {
          val (v, remain) = input.splitAt(s.length)
          Right(ParseSuccess(v, v, remain))
        } else Left(s"'${input.drop(s.length)}' does not match '$s'")
      }
      override def toString: String = s"string($s)"
    }
    override implicit def regex(r: Regex): JsonParser[String] = new JsonParser[String] {
      override def parse(input: String): ParseResult[String] = {
        val m = r.findPrefixOf(input)
        m.fold[ParseResult[String]] {
          Left(s"Regex '$r' does not match")
        } { v =>
          Right(ParseSuccess(v, v, input.drop(v.length)))
        }
      }
      override def toString: String = s"regex($r)"
    }
    override def slice[A](p: JsonParser[A]): JsonParser[String] = new JsonParser[String] {
      override def parse(input: String): ParseResult[String] = {
        p.parse(input) match {
          case Left(v) => Left(v)
          case Right(ParseSuccess(_, s, remainingInput)) => Right(ParseSuccess(s, s, remainingInput))
        }
      }
      override def toString: String = s"slice($p)"
    }
    override def or[A](p1: JsonParser[A], p2: => JsonParser[A]): JsonParser[A] = new JsonParser[A] {
      override def parse(input: String): ParseResult[A] = {
        p1.parse(input) match {
          case Left(_) => p2.parse(input)
          case r => r
        }
      }
      override def toString: String = s"or($p1,$p2)"
    }
    override def succeed[A](a: A): JsonParser[A] = new JsonParser[A] {
      override def parse(input: String): ParseResult[A] = {
        Right(ParseSuccess(a, "", input))
      }
      override def toString: String = s"succeed $a"
    }
    override def flatMap[A, B](p: JsonParser[A])(f: A => JsonParser[B]): JsonParser[B] = new JsonParser[B] {
      override def parse(input: String): ParseResult[B] = {
        p.parse(input) match {
          case Left(v) => Left(v)
          case Right(ParseSuccess(v, _, remainingInput)) => f(v).parse(remainingInput)
        }
      }
      override def toString: String = s"flatMap($p)"
    }
    override def run[A](p: JsonParser[A])(input: String): Either[ParseError, A] =
      p.parse(input) match {
        case Left(v) => Left(v)
        case Right(ParseSuccess(v, _, _)) => Right(v)
      }
  }

  val json: JsonParser[JSON] = {
    import parsers._
    import JSON._

    val quotedString = regex(""""[^"]*"""".r).map(v => v.substring(1, v.length - 1))
    val whitespace = regex("""\s+""".r)
    def trimWhitespace[A](p: JsonParser[A]) = map(product(many(whitespace), product(p, many(whitespace))))(_._2._1)
    val comma = char(',')
    val colon = char(':')

    val jsonNull = map(string("null"))(_ => JNull)
    val jsonNumber = map(regex("""[0-9.]+""".r))(v => JNumber(v.toDouble))
    val jsonString = map(quotedString)(JString)
    val jsonBool = map(or(string("true"), string("false")))(v => JBool(v == "true"))
    val primitive = or(jsonNull, or(jsonNumber, or(jsonString, jsonBool)))

    lazy val jsonArray = flatMap(char('[')) { _ =>
      lazy val _jsonValue = jsonValue
      val arrayItemComma = map(product(_jsonValue, comma))(_._1)
      val finalArrayItem = map(product(_jsonValue, char(']')))(_._1)
      val emptyArray = map(char(']'))(_ => List.empty[JSON])
      val jsonList = map(product(many(arrayItemComma), finalArrayItem))(v => v._1 :+ v._2)
      map(or(emptyArray, jsonList))(v => JArray(v.toIndexedSeq))
    }

    lazy val jsonObject: JsonParser[JObject] = flatMap(char('{')) { _ =>
      lazy val _jsonValue = jsonValue
      val objectItem = trimWhitespace(product(map(product(quotedString, trimWhitespace(colon)))(_._1), _jsonValue))
      val objectItemComma = map(product(objectItem, comma))(_._1)
      val finalObjectItem = map(product(objectItem, char('}')))(_._1)
      val emptyObject = map(char('}'))(_ => Map.empty[String, JSON])
      val obj = map(product(many(objectItemComma), finalObjectItem))(v => v._1 :+ v._2)
      map(or(emptyObject, obj))(v => JObject(v.toMap))
    }

    def jsonValue: JsonParser[JSON] = trimWhitespace(or(primitive, or(jsonArray, jsonObject)))

    jsonValue
  }

  def run(input: String): Either[ParseError, JSON] = parsers.run(json)(input)
}

sealed trait JSON extends Serializable with Product
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
