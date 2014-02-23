package argonaut

import org.scalacheck.Prop._
import Data._
import Json._
import scalaz._
import Scalaz._

object JsonSpecification extends SpecLite {
  // NOTE: List[Json] should be JsonArray, but it is failing to resolve under 2.10.0 with type alias.

  "Json" should {
    "same value should be equal" ! forAll((j: Json) =>
      j === j)
    "modified string should not be equal" ! forAll((j: JString) =>
      j.withString(_ + "test") /== j)
    "modified number should not be equal" ! forAll((j: JNumber) =>
      j.withNumber(number => if (number === 0.0d) number + 1 else number * 2) /== j)
    "modified array should not be equal" ! forAll((j: JArray) =>
      j.withArray(jEmptyArray :: _) /== j)
    "modified object should not be equal" ! forAll((j: JObject) =>
      j.withObject(_ + ("veryunlikelytoberandomlygeneratedkey", jString("veryunlikelytoberandomlygeneratedvalue"))) /== j)
    "modified boolean should not be equal" ! forAll((j: JBool) =>
      j.not /== j)
    "not compose not is id" ! forAll((j: Json) =>
      j.not.not === j)
    "no-effect not equals !isBool" ! forAll((j: Json) =>
      (j.not === j) != j.isBool)
    "effect not equals isBool" ! forAll((j: Json) =>
      (j.not /== j) === j.isBool)
    "effect withNumber implies isNumber" ! forAll((j: Json, k: JsonNumber => JsonNumber) =>
      ((j withNumber k) === j) || j.isNumber)
    "effect withString implies isString" ! forAll((j: Json, k: JsonString => JsonString) =>
      ((j withString k) === j) || j.isString)
    "effect withArray implies isArray" ! forAll((j: Json, k: List[Json] => List[Json]) =>
      ((j withArray k) === j) || j.isArray)
    "effect withObject implies isObject" ! forAll((j: Json, k: JsonObject => JsonObject) =>
      ((j withObject k) === j) || j.isObject)
    "Array prepend puts element on head" ! forAll((j: Json, e: Json) =>
      !j.isArray || (e -->>: j).array.map(_.head) === e.some)
    "jBool isBool" ! forAll((b: Boolean) =>
      jBool(b).isBool)
    "jNumber isNumber" ! forAll((n: JsonNumber) => !n.isNaN && !n.isInfinity ==>
      jNumberOrNull(n).isNumber)
    "jString isString" ! forAll((s: String) =>
      jString(s).isString)
    "jArray isArray" ! forAll((a: List[Json]) =>
      jArray(a).isArray)
    "jSingleArray is single array" ! forAll((j: Json) =>
      jSingleArray(j).array === List(j).some)
    "jObject isObject" ! forAll((a: JsonObject) =>
      jObject(a).isObject)
    "jSingleObject is single object" ! forAll((f: JsonField, j: Json) =>
      (jSingleObject(f, j).obj map (_.toList)) === List((f, j)).some)
  }
}
