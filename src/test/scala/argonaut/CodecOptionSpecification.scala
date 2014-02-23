package argonaut

import scalaz._, std.AllInstances._
import Argonaut._
import org.scalacheck.Prop.forAll

object CodecOptionSpecification extends SpecLite {
  case class Thing(value: Option[String])

  implicit def ThingCodecJson: CodecJson[Thing] =
    casecodec1(Thing.apply, Thing.unapply)("value")

  "handles missing field" ! {
    jEmptyObject.as[Thing] must_== DecodeResult.ok(Thing(None))
  }

  "handles null field" ! {
    Json.obj("value" := jNull).as[Thing] must_== DecodeResult.ok(Thing(None))
  }

  "handles set field" ! forAll{ (value: String) =>
     Json.obj("value" := value).as[Thing] must_== DecodeResult.ok(Thing(Some(value)))
  }

  "handles missing nested fields using as[T]" ! forAll{ (value: String) =>
    val third = Json.obj("first" := jEmptyObject)
      .hcursor
      .downField("first")
      .downField("second")
      .downField("third")
      .as[Option[String]]
    third must_=== DecodeResult.ok(None)
  }

  "handles missing nested fields using get[T]" ! forAll{ (value: String) =>
    val third = Json.obj("first" := jEmptyObject)
      .hcursor
      .downField("first")
      .downField("second")
      .get[Option[String]]("third")
    third must_=== DecodeResult.ok(None)
  }
}
