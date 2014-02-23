package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._

object CodecExample extends SpecLite {
  case class Person(name: String, age: Int)

  val fred = Person("Fred", 40)

  implicit val personEqual: Equal[Person] = Equal.equalA
  implicit val personShow: Show[Person] = Show.showA

  def encodeDecode(json: String)(implicit encode: EncodeJson[Person], decode: DecodeJson[Person]) = {
    val person: Option[Person] = json.decodeOption[Person]
    val encodedJson: Option[String] = person.map(_.jencode.nospaces)
    person must_=== fred.some
    encodedJson must_=== json.some
  }

  "CodecExample" should {
    "Array codec" ! {
      implicit val DecodePerson: DecodeJson[Person] =
        jdecode2(Person(_: String, _: Int))

      implicit val EncodePerson: EncodeJson[Person] =
        jencode2((p: Person) => (p.name, p.age))

      encodeDecode("""["Fred",40]""")
    }
    "Object codec" ! {
      implicit val DecodePerson: DecodeJson[Person] =
        jdecode2L(Person(_: String, _: Int))("name", "age")

      implicit val EncodePerson: EncodeJson[Person] =
        jencode2L((p: Person) => (p.name, p.age))("name", "age")

      encodeDecode("""{"name":"Fred","age":40}""")
    }
  }
}
