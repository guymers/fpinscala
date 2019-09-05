package chapter9

import org.scalatest.FreeSpec

class Chapter9Test extends FreeSpec {
  import chapter9.JSON._

  "json parser" - {
    "parse primitives" in {
      assert(JsonParser.run("null") == Right(JNull))
      assert(JsonParser.run("true") == Right(JBool(true)))
      assert(JsonParser.run("false") == Right(JBool(false)))
      assert(JsonParser.run("123") == Right(JNumber(123D)))
      assert(JsonParser.run("123.45") == Right(JNumber(123.45D)))
      assert(JsonParser.run(""""string"""") == Right(JString("string")))
    }

    "parse arrays" in {
      assert(JsonParser.run("[]") == Right(JArray(Vector.empty)))
      assert(JsonParser.run("[123]") == Right(JArray(Vector(JNumber(123D)))))
      assert(JsonParser.run("""[123, "string"]""") == Right(JArray(Vector(JNumber(123D), JString("string")))))
      assert(JsonParser.run("""[123"string"]""").isLeft)
      assert(JsonParser.run("""[123 "string"]""").isLeft)

      assert(
        JsonParser.run("""[[123, 456], ["a", "b"]]""") == Right(
          JArray(Vector(
            JArray(Vector(JNumber(123D), JNumber(456D))),
            JArray(Vector(JString("a"), JString("b")))
          ))
        )
      )
    }

    "parse objects" in {
      assert(JsonParser.run("{}") == Right(JObject(Map.empty)))
      assert(JsonParser.run("""{"key" : "value"}""") == Right(JObject(Map("key" -> JString("value")))))
      assert(
        JsonParser.run("""{"key": "value", "num": 123}""") == Right(
          JObject(Map("key" -> JString("value"), "num" -> JNumber(123D)))
        )
      )
      assert(JsonParser.run("""{"key":"value""num":123}""").isLeft)
      assert(JsonParser.run("""{"key" : "value" "num" : 123}""").isLeft)

      assert(
        JsonParser.run("""{"key":["a","b"]}""") == Right(
          JObject(Map("key" -> JArray(Vector(JString("a"), JString("b")))))
        )
      )
    }

    "parse" in {
      val json = """
        {
          "Company name": "Microsoft Corporation",
          "Ticker": "MSFT",
          "Active": true,
          "Price": 30.66,
          "Shares outstanding": 8.38e9,
          "Related companies": [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
        }
      """
      assert(JsonParser.run(json) == Right(JObject(Map(
        "Company name" -> JString("Microsoft Corporation"),
        "Ticker" -> JString("MSFT"),
        "Active" -> JBool(true),
        "Price" -> JNumber(30.66),
        "Shares outstanding" -> JNumber(8.380000000000001E9),
        "Related companies" -> JArray(Vector(JString("HPQ"), JString("IBM"), JString("YHOO"), JString("DELL"), JString("GOOG"))),
      ))))
    }
  }
}
