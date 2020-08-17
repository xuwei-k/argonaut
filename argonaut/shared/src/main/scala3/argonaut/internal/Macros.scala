package argonaut
package internal

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.deriving.{ArrayProduct, Mirror, productElement}
import scala.compiletime.{constValueTuple, erasedValue, summonFrom}

object Macros {

  inline def summonEncoder[A]: EncodeJson[A] =
    summonFrom {
      case x: EncodeJson[A] =>
        x
      case _: Mirror.ProductOf[A] =>
        Macros.derivedEncoder[A]
    }

  inline def summonDecoder[A]: DecodeJson[A] =
    summonFrom {
      case x: DecodeJson[A] =>
        x
      case _: Mirror.ProductOf[A] =>
        Macros.derivedDecoder[A]
    }

  inline def summonCodec[A]: CodecJson[A] =
    summonFrom {
      case x: CodecJson[A] =>
        x
      case _: Mirror.ProductOf[A] =>
        Macros.derivedCodec[A]
    }

  inline def summonDecoders[T <: Tuple]: Tuple.Map[T, DecodeJson] =
    inline erasedValue[T] match {
      case _: EmptyTuple =>
        EmptyTuple
      case _: (t *: ts) =>
        summonDecoder[t] *: summonDecoders[ts]
    }

  inline def summonEncoders[T <: Tuple]: Tuple.Map[T, EncodeJson] =
    inline erasedValue[T] match {
      case _: EmptyTuple =>
        EmptyTuple
      case _: (t *: ts) =>
        summonEncoder[t] *: summonEncoders[ts]
    }

  inline def derivedEncoder[A](using inline A: Mirror.ProductOf[A]): EncodeJson[A] =
    new EncodeJson[A] {
      private[this] val elemEncoders: Tuple.Map[A.MirroredElemTypes, EncodeJson] =
        Macros.summonEncoders[A.MirroredElemTypes]

      override def encode(a: A): Json =
        Json.jObject(
          createJsonObject(a.asInstanceOf[Product])
        )

      private[this] def createJsonObject(value: Product): JsonObject = {
        def encodeWith(index: Int)(p: Any): (String, Json) = {
          (value.productElementName(index), productElement[EncodeJson[Any]](elemEncoders, index).apply(p))
        }
        val elems: Iterator[Any] = value.productIterator
        @tailrec def loop(i: Int, acc: JsonObject): JsonObject = {
          if (elems.hasNext) {
            val field = encodeWith(i)(elems.next())
            loop(i + 1, acc :+ field)
          } else {
            acc
          }
        }
        loop(0, JsonObject.empty)
      }
    }

  inline def derivedCodec[A](using inline A: Mirror.ProductOf[A]): CodecJson[A] =
    CodecJson.derived[A](
      E = derivedEncoder[A],
      D = derivedDecoder[A],
    )

  inline def derivedDecoder[A](using inline A: Mirror.ProductOf[A]): DecodeJson[A] =
    new DecodeJson[A] {
      private[this] def decodeWith(index: Int)(c: HCursor): DecodeResult[AnyRef] = {
        productElement[DecodeJson[AnyRef]](elemDecoders, index).tryDecode(c.downField(productElement(elemLabels, index)))
      }

      private[this] def resultIterator(c: HCursor): Iterator[DecodeResult[AnyRef]] =
        new AbstractIterator[DecodeResult[AnyRef]] {
          private[this] var i: Int = 0

          def hasNext: Boolean = i < elemCount

          def next: DecodeResult[AnyRef] = {
            val result = decodeWith(i)(c)
            i += 1
            result
          }
        }

      private[this] val elemLabels: Tuple.Widen[A.MirroredElemLabels] =
        constValueTuple[A.MirroredElemLabels]

      private[this] val elemDecoders: Tuple.Map[A.MirroredElemTypes, DecodeJson] =
        Macros.summonDecoders[A.MirroredElemTypes]

      private[this] val elemCount = elemDecoders.size

      override def decode(c: HCursor): DecodeResult[A] = {
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
