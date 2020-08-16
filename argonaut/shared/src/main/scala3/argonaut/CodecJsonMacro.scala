package argonaut

import argonaut.internal.CodecDerivation

trait CodecJsonMacro {
  inline def derive[A]: CodecJson[A] =
    internal.Macros.summonCodec[A]
}
