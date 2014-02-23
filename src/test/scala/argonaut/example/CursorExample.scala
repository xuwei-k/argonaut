package argonaut.example

import argonaut._, Argonaut._
import scalaz._, Scalaz._

object CursorExample extends SpecLite {
  val json =
      """
        {
          "abc" :
            {
              "def" : 7
            },
          "ghi" :
            {
              "ata" : null,
              "jkl" :
                {
                  "mno" : "argo"
                }
            },
          "pqr" : false,
          "operator": "is",
          "values": [
                      ["cat", "lol"]
                    , "dog"
                    , "rabbit"
                    ],
          "xyz" : 24
        }
      """

  "CursorExample" should {
    """Replace '["cat", "lol"]' with 'false'""" ! {
      json.parseOption.flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (_ := jBool(false)) map (-_)
      ).isDefined must_=== true
    }
    "Visit the 'values' array" ! {
      json.parseOption.flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (-_)
      ).isDefined must_=== true
    }
    """Delete the element '"dog"' from the 'values' array.""" ! {
      json.parseOption.flatMap (k =>
        +k --\ "values" flatMap (_.downArray) flatMap (_.right) flatMap (!_) map (-_)
      ).isDefined must_=== true
    }
    """Replace '["cat", "lol"]' with 'false' and '"rabbit"' with 'true'""" ! {
      json.parseOption.flatMap (k =>
        +k --\ "values" flatMap (_.downArray) map (_ := jBool(false)) flatMap (_.right) flatMap (_.right) map (_ := jBool(true)) map (-_)
      ).isDefined must_=== true
    }
  }
}
