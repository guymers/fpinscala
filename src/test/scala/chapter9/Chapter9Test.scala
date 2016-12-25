package chapter9

import org.scalatest.{EitherValues, FlatSpec}

class Chapter9Test extends FlatSpec with EitherValues {
  import chapter9.JSON._

  "json parser" should "parse primitives" in {
    assert(JsonParser.run("null").right.value === JNull)
    assert(JsonParser.run("true").right.value === JBool(true))
    assert(JsonParser.run("false").right.value === JBool(false))
    assert(JsonParser.run("123").right.value === JNumber(123D))
    assert(JsonParser.run("123.45").right.value === JNumber(123.45D))
    assert(JsonParser.run(""""string"""").right.value === JString("string"))
  }

  "json parser" should "parse arrays" in {
    assert(JsonParser.run("[]").right.value === JArray(Vector.empty))
    assert(JsonParser.run("[123]").right.value === JArray(Vector(JNumber(123D))))
    assert(JsonParser.run("""[123, "string"]""").right.value === JArray(Vector(JNumber(123D), JString("string"))))
    assert(JsonParser.run("""[123"string"]""").isLeft)
    assert(JsonParser.run("""[123 "string"]""").isLeft)

    assert(
      JsonParser.run("""[[123, 456], ["a", "b"]]""").right.value ===
        JArray(Vector(
          JArray(Vector(JNumber(123D), JNumber(456D))),
          JArray(Vector(JString("a"), JString("b")))
        ))
    )
  }

  "json parser" should "parse objects" in {
    assert(JsonParser.run("{}").right.value === JObject(Map.empty))
    assert(JsonParser.run("""{"key" : "value"}""").right.value === JObject(Map("key" -> JString("value"))))
    assert(
      JsonParser.run("""{"key": "value", "num": 123}""").right.value ===
        JObject(Map("key" -> JString("value"), "num" -> JNumber(123D)))
    )
    assert(JsonParser.run("""{"key":"value""num":123}""").isLeft)
    assert(JsonParser.run("""{"key" : "value" "num" : 123}""").isLeft)

    assert(
      JsonParser.run("""{"key":["a","b"]}""").right.value ===
        JObject(Map("key" -> JArray(Vector(JString("a"), JString("b")))))
    )
  }
}
