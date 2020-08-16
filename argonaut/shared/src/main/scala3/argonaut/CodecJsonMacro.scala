package argonaut

trait CodecJsonMacro {
  inline def derive[A]: CodecJson[A] =
    internal.Macros.summonCodec[A]
}
