package argonaut

object TestMain {
  case class Foo(a: Int, b: String, c: Boolean)

  def main(args: Array[String]): Unit = {
    val d1 = argonaut.DecodeJson.derive[Foo]
    val e1 = argonaut.EncodeJson.derive[Foo]

    test(d1, e1)
 
    val c = argonaut.CodecJson.derive[Foo]
    test(d1, e1)
  }
    
  def test(d: DecodeJson[Foo], e: EncodeJson[Foo]) = {
      
    val foo = Foo(3, "a", true)
    val json = e.apply(foo)
    assert(json == Json.obj(
    "a" -> Json.jNumber(3),
      "b" -> Json.jString("a"),
      "c" -> Json.jBool(true),
    ))
    
    assert(Right(foo) == d.decodeJson(json).result)
  }

}
