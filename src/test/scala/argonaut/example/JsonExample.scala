package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._

object JsonExample extends SpecLite {
  val json =
    Json(
      "name" := "fred",
      "age" := 23,
      "wallet" := List(
        Json { "value" := 100 },
        Json { "value" := 10 },
        Json { "value" := 50 }
      )
    )

  val value =
    Person("fred", 23, List(Coin(100), Coin(10), Coin(50)))

  case class Coin(value: Int)
  case class Person(name: String, age: Int, wallet: List[Coin])

  implicit val CodecCoin = casecodec1(Coin.apply, Coin.unapply)("value")
  implicit val CodecPerson = casecodec3(Person.apply, Person.unapply)("name", "age", "wallet")

  "JsonExample" should {
    "Can decode hand crafted object" ! {
      json.as[Person].toOption must_== Some(value)
    }
    "Can encode to match hand crafted object" ! {
      value.asJson must_=== json
    }
  }
}
