package argonaut
package internal

import scala.collection.{AbstractIterable, AbstractIterator}
import scala.deriving.{ArrayProduct, Mirror}
import scala.compiletime.{constValue, erasedValue, summonFrom}

object Macros {
  inline final def summonLabels[T <: Tuple]: Array[String] = summonLabelsRec[T].toArray

  inline final def summonDecoders[T <: Tuple]: Array[DecodeJson[_]] = summonDecodersRec[T].toArray

  inline final def summonEncoders[T <: Tuple]: Array[EncodeJson[_]] = summonEncodersRec[T].toArray

  inline final def summonEncoder[A]: EncodeJson[A] = summonFrom {
    case encodeA: EncodeJson[A] => encodeA
    case _: Mirror.ProductOf[A] => Macros.derivedEncoder[A]
  }

  inline final def summonDecoder[A]: DecodeJson[A] = summonFrom {
    case decodeA: DecodeJson[A] => decodeA
    case _: Mirror.ProductOf[A] => Macros.derivedDecoder[A]
  }

  inline final def summonCodec[A]: CodecJson[A] = summonFrom {
    case codecA: CodecJson[A] => codecA
    case _: Mirror.ProductOf[A] => Macros.derivedCodec[A]
  }

  inline final def summonLabelsRec[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonLabelsRec[ts]
    }

  inline final def summonDecodersRec[T <: Tuple]: List[DecodeJson[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonDecoder[t] :: summonDecodersRec[ts]
    }

  inline final def summonEncodersRec[T <: Tuple]: List[EncodeJson[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonEncoder[t] :: summonEncodersRec[ts]
    }

  inline final def derivedEncoder[A](using inline A: Mirror.ProductOf[A]): EncodeJson[A] =
    new EncodeJson[A] {
      private[this] val elemEncoders: Array[EncodeJson[_]] =
        Macros.summonEncoders[A.MirroredElemTypes]
   
      override final def encode(a: A): Json =
        Json.jObject(
          JsonObject.fromIterable(encodedIterable(a.asInstanceOf[Product]))
        )

      private[this] final def encodedIterable(value: Product): Iterable[(String, Json)] =
        new AbstractIterable[(String, Json)] {
          private[this] def encodeWith(index: Int)(p: Any): (String, Json) =
            (value.productElementName(index), elemEncoders(index).asInstanceOf[EncodeJson[Any]].apply(p))

          def iterator: Iterator[(String, Json)] =
            new scala.collection.AbstractIterator[(String, Json)] {
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

  inline final def derivedCodec[A](using inline A: Mirror.ProductOf[A]): CodecJson[A] =
    CodecJson.derived[A](
      E = derivedEncoder[A],
      D = derivedDecoder[A],
    )

  inline final def derivedDecoder[A](using inline A: Mirror.ProductOf[A]): DecodeJson[A] =
    new DecodeJson[A] {
      final def decodeWith(index: Int)(c: HCursor): DecodeResult[AnyRef] =
        elemDecoders(index).asInstanceOf[DecodeJson[AnyRef]].tryDecode(c.downField(elemLabels(index)))

      final def resultIterator(c: HCursor): Iterator[DecodeResult[AnyRef]] =
        new Iterator[DecodeResult[AnyRef]] {
          private[this] var i: Int = 0

          def hasNext: Boolean = i < elemCount

          def next: DecodeResult[AnyRef] = {
            val result = decodeWith(i)(c)
            i += 1
            result
          }
        }

      private[this] val elemLabels = Macros.summonLabels[A.MirroredElemLabels]

      private[this] val elemDecoders: Array[DecodeJson[_]] =
        Macros.summonDecoders[A.MirroredElemTypes]

      private[this] val elemCount = elemDecoders.size
      
      final def decode(c: HCursor): DecodeResult[A] = {
        DecodeResult[A] {
          val iter = resultIterator(c)
          val res = new Array[AnyRef](elemCount)
          var failed: (String, CursorHistory) = null
          var i: Int = 0

          while (iter.hasNext && (failed eq null)) {
            iter.next.result match {
              case Right(value) =>
                res(i) = value
              case Left(l) =>
                failed = l
            }
            i += 1
          }

          if (failed eq null) {
            Right(A.fromProduct(new ArrayProduct(res)))
          } else {
            Left(failed)
          }
        }
      }
    }
}
