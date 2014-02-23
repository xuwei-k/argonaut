package argonaut

import argonaut.Argonaut._
import scalaz._, Scalaz._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import Data._

object StringWrapSpecification extends SpecLite {
  case class Person(name: String, age: Int)

  implicit val DecodePerson: DecodeJson[Person] =
    jdecode2L(Person(_: String, _: Int))("name", "age").setName("Person")

  implicit val EqualPerson: Equal[Person] = Equal.equalA[Person]
  implicit val ShowPerson: Show[Person] = Show.showA[Person]

  val validJSONTemplate: String = """{"name":"%s","age":"%s"}"""

  val invalidJSONTemplate: String = """{"name":"%s","ag":"%s"}"""

  "parse" should {
    "returns a success wrapped Json for valid JSON" ! forAll{(json: Json) =>
      json.nospaces.parse must_=== json.right[String]
    }
    "returns a failure for invalid JSON" ! {
  "{".parse must_=== "JSON terminates unexpectedly".left
    }
  }

  "parseWith[X](Json => X, String => X): X" should {
    "returns the transformed Json for valid JSON" ! forAll{(json: Json) =>
      json.nospaces.parseWith(_.some, _ => None) must_=== json.some
    }
    "returns a failure for invalid JSON" ! {
      "{".parseWith(_ => "Oops", identity) must_=== "JSON terminates unexpectedly"
    }
  }

  """decode[X: DecodeJson]: String \/ X""" should {
    "returns the decoded Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = validJSONTemplate.format(name, age)
      json.decode[Person] must_=== Person(name, age).right
    }
    "returns a failure for invalid JSON" ! {
      "{".parseWith(_ => "Oops", identity) must_=== "JSON terminates unexpectedly"
    }
    "returns a failure for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = invalidJSONTemplate.format(name, age)
      json.decode[Person].swapped(_.map(_.map(_._1))) must_==  "Person".right.left
    }
  }

  "decodeWith[A, X: DecodeJson](X => A, String => A, (String, CursorHistory) => A): A" should {
    "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = validJSONTemplate.format(name, age)
      json.decodeWith[Option[Person], Person](_.some, _ => None, (_, _) => None) must_=== Person(name, age).some
    }
    "returns the result of the parseFailure function for invalid JSON" ! {
      "{".decodeWith[Option[Person], Person](_ => None, _ => Person("Test", 5).some, (_, _) => None) must_=== Person("Test", 5).some
    }
    "returns the result of the decodeFailure function for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = invalidJSONTemplate.format(name, age)
      json.decodeWith[Option[Person], Person](_ => None, _ => None, (_, _) => Person("Test", 5).some) must_=== Person("Test", 5).some
    }
  }

  "decodeOr[A, X: DecodeJson](X => A, => A): A" should {
    "returns the decoded and transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = validJSONTemplate.format(name, age)
      json.decodeOr[Option[Person], Person](_.some, None) must_=== Person(name, age).some
    }
    "returns the result of the default function for invalid JSON" ! {
      "{".decodeOr[Option[Person], Person](_ => None, Person("Test", 5).some) must_=== Person("Test", 5).some
    }
    "returns the result of the default function for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = invalidJSONTemplate.format(name, age)
      json.decodeOr[Option[Person], Person](_ => None, Person("Test", 5).some) must_=== Person("Test", 5).some
    }
  }

  "parseOr[X](Json => X, => X): X" should {
    "returns the transformed Json for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = validJSONTemplate.format(name, age)
      json.parseOr[Option[Json]](_.some, None) must_=== Some(("age", jString(age.toString)) ->: ("name", jString(name)) ->: jEmptyObject)
    }
    "returns the result of the failure function for invalid JSON" ! {
      "{".parseOr[String](_ => "It works!", "Failure") must_=== "Failure"
    }
  }

  "parseOption: Option[Json]" should {
    "returns Json wrapped in Some for valid JSON" ! forAll{(json: Json) =>
      json.nospaces.parseOption must_=== json.some
    }
    "returns a failure for invalid JSON" ! {
      "{".parseOption must_=== None
    }
  }

  "decodeOption[X: DecodeJson]: Option[X]" should {
    "returns the decoded value wrapped in a Some for valid JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = validJSONTemplate.format(name, age)
      json.decodeOption[Person] must_=== Person(name, age).some
    }
    "returns a None for invalid JSON" ! {
      "{".decodeOption[Person] must_=== None
    }
    "returns a None for undecodable JSON" ! forAllNoShrink(alphaStr, arbitrary[Int]){(name: String, age: Int) =>
      val json = invalidJSONTemplate.format(name, age)
      json.decodeOption[Person] must_=== None
    }
  }
}
