package argonaut

import org.scalacheck._, Prop.forAll
import Argonaut._

object CodecNumberSpecification extends Properties("Codec Numbers") {

  property("double that is not NaN or infinity encodes to number") = forAll{ xs: List[Double] =>
    xs.filter(x => !x.isNaN && !x.isInfinity).asJson.array.forall(_.forall(_.isNumber))
  }

  property("int always encodes to number") = forAll{ xs: List[Int] =>
    xs.asJson.array.forall(_.forall(_.isNumber))
  }

  property("long always encodes to string") = forAll{ xs: List[Long] =>
    xs.asJson.array.forall(_.forall(_.isString))
  }

}
