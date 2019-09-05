package chapter9

import chapter8.{Gen, Prop}

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>

  val digits: Parser[Int] = regex("[0-9]+".r).map(_.toInt)

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  // 9.1
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p1, p2).map { case (a, b) => f(a, b) }
  }

  def many1[A](p: Parser[A]): Parser[::[A]] = map2(p, many(p))(::(_, _))

  // 9.2 laws

  // 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) or succeed(Nil)
  }

  // 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  // 9.6
  def numChars[A](p: Parser[A]): Parser[List[A]] = {
    digits.flatMap(n => listOfN(n, p))
  }

  // 9.7
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    p1.flatMap(a => p2.map(b => (a, b)))
  }

  def map2FlatMap[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    p1.flatMap(a => p2.map(b => f(a, b)))
  }

  // 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
    p.flatMap(a => succeed(f(a)))
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
    override def succeed[A](a: A): JsonParser[A] = new JsonParser[A] {
      override def parse(input: String): ParseResult[A] = {
        Right(ParseSuccess(a, "", input))
      }
      override def toString: String = s"succeed($a)"
    }
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
        r.findPrefixOf(input).toRight(s"Regex '$r' does not match").map { v =>
          ParseSuccess(v, v, input.drop(v.length))
        }
      }
      override def toString: String = s"regex($r)"
    }
    override def slice[A](p: JsonParser[A]): JsonParser[String] = new JsonParser[String] {
      override def parse(input: String): ParseResult[String] = {
        p.parse(input).map { case ParseSuccess(_, s, remainingInput) =>
          ParseSuccess(s, s, remainingInput)
        }
      }
      override def toString: String = s"slice($p)"
    }
    override def or[A](p1: JsonParser[A], p2: => JsonParser[A]): JsonParser[A] = new JsonParser[A] {
      override def parse(input: String): ParseResult[A] = {
        p1.parse(input).left.flatMap(_ => p2.parse(input))
      }
      override def toString: String = s"or($p1,$p2)"
    }
    override def flatMap[A, B](p: JsonParser[A])(f: A => JsonParser[B]): JsonParser[B] = new JsonParser[B] {
      override def parse(input: String): ParseResult[B] = {
        p.parse(input).flatMap { case ParseSuccess(v, _, remainingInput) =>
          f(v).parse(remainingInput)
        }
      }
      override def toString: String = s"flatMap($p)"
    }
    override def run[A](p: JsonParser[A])(input: String): Either[ParseError, A] =
      p.parse(input).map(_.value)
  }

  val json: JsonParser[JSON] = {
    import parsers._
    import JSON._

    val quotedString = regex(""""[^"]*"""".r).map(v => v.substring(1, v.length - 1))
    val whitespace = regex("""\s+""".r)
    def trimWhitespace[A](p: JsonParser[A]) = map(product(many(whitespace), product(p, many(whitespace))))(_._2._1)
    val comma = char(',')
    val colon = char(':')

    val jsonNull = string("null").map(_ => JNull)
    val jsonNumber = regex("""[0-9.]+""".r).flatMap { s =>
      val d = s.toDouble
      regex("""e[0-9]+""".r).map(s => s.drop(1).toInt).or(succeed(0)).map { e =>
        d * Math.pow(10, e.toDouble)
      }
    }.map(JNumber)
    val jsonString = quotedString.map(JString)
    val jsonBool = (string("true") or string("false")).map(v => JBool(v == "true"))
    val primitive = jsonNull or jsonNumber or jsonString or jsonBool

    lazy val jsonArray = flatMap(char('[')) { _ =>
      lazy val _jsonValue = jsonValue
      val arrayItemComma = product(_jsonValue, comma).map(_._1)
      val finalArrayItem = product(_jsonValue, char(']')).map(_._1)
      val emptyArray = char(']').map(_ => List.empty[JSON])
      val jsonList = product(many(arrayItemComma), finalArrayItem).map(v => v._1 :+ v._2)
      (emptyArray or jsonList).map(v => JArray(v.toIndexedSeq))
    }

    lazy val jsonObject: JsonParser[JObject] = flatMap(char('{')) { _ =>
      lazy val _jsonValue = jsonValue
      val objectItem = trimWhitespace(product(map(product(quotedString, trimWhitespace(colon)))(_._1), _jsonValue))
      val objectItemComma = product(objectItem, comma).map(_._1)
      val finalObjectItem = product(objectItem, char('}')).map(_._1)
      val emptyObject = char('}').map(_ => Map.empty[String, JSON])
      val obj = product(many(objectItemComma), finalObjectItem).map(v => v._1 :+ v._2)
      (emptyObject or obj).map(v => JObject(v.toMap))
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
