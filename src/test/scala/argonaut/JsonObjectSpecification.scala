package argonaut

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import Data._
import scalaz._, syntax.show._

object JsonObjectSpecification extends Properties("JsonObject") {

  property("fields") = forAll{ o: JsonObject =>
    o.fields.length == o.fieldSet.size
  }

  property("shows") = forAll{ o: JsonObject =>
    o.shows == o.toString
  }

}
