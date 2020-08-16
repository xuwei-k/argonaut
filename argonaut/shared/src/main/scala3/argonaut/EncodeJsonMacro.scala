package argonaut

trait EncodeJsonMacro {
  inline def derive[A]: EncodeJson[A] = 
    internal.Macros.summonEncoder[A]
}
