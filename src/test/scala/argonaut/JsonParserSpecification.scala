package argonaut

import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scalaz.Success
import org.scalacheck._
import org.scalacheck.Shrink._
import scalaz._
import Scalaz._
import Data._
import Argonaut._

object JsonParserSpecification extends SpecLite {
  // Generates chunks of whitespace according to the not at all specified JSON specification.
  val whitespaceGen: Gen[String] = listOf(Gen.oneOf(' ', '\n', '\r', '\t')).map(_.mkString)

  val whitespaceObjectGen: Gen[String] = whitespaceGen.map(whitespace => """#{#"field1"#:#12#,#"field2"#:#"test"#}#""".replace("#", whitespace))
  val whitespaceObject: Json = ("field1" := 12.0d) ->: ("field2" := "test") ->: jEmptyObject

  val whitespaceArrayGen: Gen[String] = whitespaceGen.map(whitespace => """#[#"value1"#,#12#]#""".replace("#", whitespace))
  val whitespaceArray: Json = jArray(jString("value1") :: jNumberOrNull(12) :: Nil)

  "Whitespace is handled correctly for an object" ! forAllNoShrink(whitespaceObjectGen){json =>
    val parseResult = JsonParser.parse(json)
    ("parseResult = " + parseResult) |:
    ("whitespaceObject = " + whitespaceObject) |:
    (parseResult must_=== whitespaceObject.right[String])
  }

  "Whitespace is handled correctly for an array" ! forAllNoShrink(whitespaceArrayGen){json =>
    val parseResult = JsonParser.parse(json)
    ("parseResult = " + parseResult) |:
    ("whitespaceArray = " + whitespaceArray) |:
    (parseResult must_=== whitespaceArray.right[String])
  }

  "Valid JSON parses into expected values" ! {
    KnownResults.validResultPairings.foreach{ case (json, expectedJSONValue) =>
      val actualParseResult = JsonParser.parse(json)
      actualParseResult must_=== expectedJSONValue.right[String]
    }
  }

  "Invalid JSON parses into expected failures" ! {
    KnownResults.parseFailures.foreach{ case (json, parseResult) =>
      val actualParseResult = JsonParser.parse(json)
      actualParseResult must_=== parseResult
    }
  }

  "Printed and then parsed again generates the same structure" ! forAll{(json: Json) =>
    val printedJSON = json.nospaces
    ("printedJSON = " + printedJSON) |: {
      val parsed = printedJSON.parse
      ("parsed = " + parsed) |: (parsed must_=== json.right)
    }
  }

}
