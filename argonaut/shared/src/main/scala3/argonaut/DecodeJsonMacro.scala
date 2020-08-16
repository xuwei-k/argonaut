package argonaut

trait DecodeJsonMacro {
  inline def derive[A]: DecodeJson[A] =
    internal.Macros.summonDecoder[A]
}
