package argonaut
package internal

import scala.quoted._
import scala.quoted.util._

import scala.deriving._
import scala.collection.mutable.WrappedArray
import scala.compiletime.{constValue, erasedValue, error, summonFrom, summonInline}


inline def summonAll[T <: Tuple]: List[EncodeJson[_]] = {
  inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[EncodeJson[t]] :: summonAll[ts]
  }
}

object Macros {
  inline final def summonLabels[T <: Tuple]: Array[String] = summonLabelsRec[T].toArray
  inline final def summonDecoders[T <: Tuple]: Array[DecodeJson[_]] = summonDecodersRec[T].toArray
  inline final def summonEncoders[T <: Tuple]: Array[EncodeJson[_]] = summonEncodersRec[T].toArray

  inline final def summonEncoder[A]: EncodeJson[A] = summonFrom {
    case encodeA: EncodeJson[A] => encodeA
    case _: Mirror.Of[A] => Encoder.AsObject.derived[A]
  }

  inline final def summonDecoder[A]: DecodeJson[A] = summonFrom {
    case decodeA: DecodeJson[A] => decodeA
    case _: Mirror.Of[A] => Decoder.derived[A]
  }

  inline final def summonLabelsRec[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: Unit => Nil
    case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]
  }

  inline final def summonDecodersRec[T <: Tuple]: List[DecodeJson[_]] =
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: (t *: ts) => summonDecoder[t] :: summonDecodersRec[ts]
    }

  inline final def summonEncodersRec[T <: Tuple]: List[EncodeJson[_]] =
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: (t *: ts) => summonEncoder[t] :: summonEncodersRec[ts]
    }

  inline final def derived[A](using inline A: Mirror.Of[A]): EncodeJson[A] =
    new DerivedEncoder[A] with DerivedInstance[A](
  constValue[A.MirroredLabel],
  Derivation.summonLabels[A.MirroredElemLabels]
  ) {
    protected[this] lazy val elemEncoders: Array[Encoder[_]] =
      Derivation.summonEncoders[A.MirroredElemTypes]

    final def encodeObject(a: A): JsonObject = inline A match {
      case m: Mirror.ProductOf[A] =>
        JsonObject.fromIterable(encodedIterable(a.asInstanceOf[Product]))
      case m: Mirror.SumOf[A] => encodeWith(m.ordinal(a))(a) match {
        case (k, v) => JsonObject.singleton(k, v)
      }
    }
  }
  
  inline given derived[T](using inline m: Mirror.ProductOf[T]) as EncodeJson[T] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    encodeProduct(p, elemInstances)
  }

  def deriveImpl[A](using tpe: Type[A], qctx: QuoteContext): Expr[EncodeJson[A]] = {
    import qctx.tasty._

    val fields: List[qctx.tasty.Symbol] = tpe.unseal.tpe.classSymbol.get.caseFields
    val fieldTypes1: List[TypeTree] = fields.collect{ case t: ValDef => t.tpt }
    val fieldTypes: List[Type] = fields.collect{ case t: ValDef => t.tpt.tpe }
    assert(fieldTypes.sizeIs == fields.size, s"${fields} ${fieldTypes}")
    val fieldNames: List[String] = fields.map(_.name)
    val fieldCount = fields.size
    val methodName = Expr("jencode" + fieldCount.toString + "L")
    
    val typeParams = tpe.unseal.tpe :: fieldTypes
    
    ???
  }

}
