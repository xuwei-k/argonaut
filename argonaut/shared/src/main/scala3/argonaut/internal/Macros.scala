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
  inline final def summonDecoders[T <: Tuple]: Array[DecodeJson[_]] = summonDecodersRec[T].toArray

  inline final def summonEncoders[T <: Tuple]: Array[EncodeJson[_]] = summonEncodersRec[T].toArray

  inline final def summonEncoder[A]: EncodeJson[A] = summonFrom {
    case encodeA: EncodeJson[A] => encodeA
    case _: Mirror.ProductOf[A] => EncoderDerivation.derived[A]
  }

  inline final def summonDecoder[A]: DecodeJson[A] = summonFrom {
    case decodeA: DecodeJson[A] => decodeA
    case _: Mirror.Of[A] => DecodeJson.derive[A]
  }

  inline final def summonDecodersRec[T <: Tuple]: List[DecodeJson[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple.type => Nil
      case _: (t *: ts) => summonDecoder[t] :: summonDecodersRec[ts]
    }

  inline final def summonEncodersRec[T <: Tuple]: List[EncodeJson[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple.type => Nil
      case _: (t *: ts) => summonEncoder[t] :: summonEncodersRec[ts]
    }
}

trait EncoderDerivation {
  inline final def derived[A](using inline A: Mirror.ProductOf[A]): EncodeJson[A] =
    new EncodeJson[A] with DerivedEncoder[A] {
      override val elemEncoders: Array[EncodeJson[_]] =
        Macros.summonEncoders[A.MirroredElemTypes]
   
      final def encode(a: A): Json = {
        Json.jObject(
          JsonObject.fromIterable(encodedIterable(a.asInstanceOf[Product]))
        )
      }
    }
}

object EncoderDerivation extends EncoderDerivation

trait DerivedEncoder[A] extends EncodeJson[A] {
  protected[this] def elemEncoders: Array[EncodeJson[_]]

  final def encodedIterable(value: Product): Iterable[(String, Json)] =
    new Iterable[(String, Json)] {
      private[this] def encodeWith(index: Int)(p: Any): (String, Json) =
        (value.productElementName(index), elemEncoders(index).asInstanceOf[EncodeJson[Any]].apply(p))

      def iterator: Iterator[(String, Json)] = new scala.collection.AbstractIterator[(String, Json)] {
        private[this] val elems: Iterator[Any] = value.productIterator
        private[this] var index: Int = 0
        def hasNext: Boolean = elems.hasNext
        def next(): (String, Json) = {
          val field = encodeWith(index)(elems.next())
          index += 1
          field
        }
      }
    }
}


