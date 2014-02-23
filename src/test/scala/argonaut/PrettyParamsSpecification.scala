package argonaut

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Gen
import Data._
import Argonaut._
import scalaz._, std.AllInstances._, syntax.equal._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._


object PrettyParamsSpecification extends SpecLite {
  // Synthetic Equal implementations used for testing.
  implicit val intToStringEqual: Equal[Int => String] = new Equal[Int => String] {
    val indents: List[Int] = (0 to 5).toList :+ 100
    def equal(a1: Int => String, a2: Int => String): Boolean = {
      indents.map(a1) === indents.map(a2)
    }
  }
  implicit val prettyParamsEqual: Equal[PrettyParams] = new Equal[PrettyParams] {
    def equal(a1: PrettyParams, a2: PrettyParams): Boolean = {
      (a1.lbraceLeft === a2.lbraceLeft) &&
      (a1.lbraceRight === a2.lbraceRight) &&
      (a1.rbraceLeft === a2.rbraceLeft) &&
      (a1.rbraceRight === a2.rbraceRight) &&
      (a1.lbracketLeft === a2.lbracketLeft) &&
      (a1.lbracketRight === a2.lbracketRight) &&
      (a1.rbracketLeft === a2.rbracketLeft) &&
      (a1.rbracketRight === a2.rbracketRight) &&
      (a1.commaLeft === a2.commaLeft) &&
      (a1.commaRight === a2.commaRight) &&
      (a1.colonLeft === a2.colonLeft) &&
      (a1.colonRight === a2.colonRight) &&
      (a1.preserveOrder === a2.preserveOrder)
    }
  }

  val jsonSpacesMap: Map[Int, String] = Map(
    0 -> """{"key1":"value1","key2":[9,21,0]}""",
    2 -> """|{
            |  "key1" : "value1",
            |  "key2" : [
            |    9,
            |    21,
            |    0
            |  ]
            |}""".stripMargin,
    4 -> """|{
            |    "key1" : "value1",
            |    "key2" : [
            |        9,
            |        21,
            |        0
            |    ]
            |}""".stripMargin
  )

  "lbraceLeft" should{
    "lens laws" ! lens.laws(PrettyParams.lbraceLeftL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = lbraceLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) must_=== """%s{"test":"value"}""".format(indent)
    }
  }
  "lbraceRightL" should{
    "lens laws" ! lens.laws(PrettyParams.lbraceRightL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = lbraceRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) must_=== """{%s"test":"value"}""".format(indent)
    }
  }
  "rbraceLeftL" should{
    "lens laws" ! lens.laws(PrettyParams.rbraceLeftL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = rbraceLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) must_=== """{"test":"value"%s}""".format(indent)
    }
  }
  "rbraceRightL" should{
    "lens laws" ! lens.laws(PrettyParams.rbraceRightL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = rbraceRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) must_=== """{"test":"value"}%s""".format(indent)
    }
  }
  "lbracketLeftL" should{
    "lens laws" ! lens.laws(PrettyParams.lbracketLeftL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = lbracketLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) must_=== """%s[true,false]""".format(indent)
    }
  }
  "lbracketRightL" should{
    "lens laws" ! lens.laws(PrettyParams.lbracketRightL)
     "indent" ! forAll{(indent: String) =>
      val prettyParams = lbracketRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) must_=== """[%strue,false]""".format(indent)
    }
  }
  "rbracketLeftL" should{
    "lens laws" ! lens.laws(PrettyParams.lbracketRightL)
     "indent" ! forAll{(indent: String) =>
      val prettyParams = rbracketLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) must_=== """[true,false%s]""".format(indent)
    }
  }
  "rbracketRightL" should{
    "lens laws" ! lens.laws(PrettyParams.rbracketRightL)
     "indent" ! forAll{(indent: String) =>
      val prettyParams = rbracketRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) must_=== """[true,false]%s""".format(indent)
    }
  }
  "commaLeftL" should{
    "lens laws" ! lens.laws(PrettyParams.commaLeftL)
     "indent" ! forAll{(indent: String) =>
      val prettyParams = commaLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) must_=== """[true%s,false]""".format(indent)
    }
  }
  "commaRightL" should{
    "lens laws" ! lens.laws(PrettyParams.commaRightL)
     "indent" ! forAll{(indent: String) =>
      val prettyParams = commaRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(jArray(List(jTrue, jFalse))) must_=== """[true,%sfalse]""".format(indent)
    }
  }
  "colonLeftL" should{
    "lens laws" ! lens.laws(PrettyParams.colonLeftL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = colonLeftL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) must_=== """{"test"%s:"value"}""".format(indent)
    }
  }
  "colonRightL" should{
    "lens laws" ! lens.laws(PrettyParams.colonRightL)
    "indent" ! forAll{(indent: String) =>
      val prettyParams = colonRightL.set(PrettyParams.nospace, _ => indent)
      prettyParams.pretty(("test" := "value") ->: jEmptyObject) must_=== """{"test":%s"value"}""".format(indent)
    }
  }
  "preserveOrderL" should{
    "lens laws" ! lens.laws(PrettyParams.preserveOrderL)
    "order preservation" ! forAll{(preserve: Boolean, pairs: List[(JsonField, Json)]) =>
      val prettyParams = preserveOrderL.set(PrettyParams.nospace, preserve)
      val json = prettyParams.pretty(jObjectAssocList(pairs)).parseOption.get
      if (preserve) {
        json.objectOrEmpty.toInsertionMap must_=== InsertionMap.apply(pairs: _*)
      } else {
        json.objectOrEmpty.toMap must_=== pairs.toMap
      }
    }
  }
  "nospaces/spaces2/spaces4" ! forAllNoShrink(Gen.oneOf(0, 2, 4), Gen.oneOf(0, 2, 4)){(firstIndex, secondIndex) =>
    val json = jsonSpacesMap(firstIndex).parseOption.get
    val printedJson = secondIndex match {
      case 0 => json.nospaces
      case 2 => json.spaces2
      case 4 => json.spaces4
    }
    printedJson must_=== jsonSpacesMap(secondIndex)
  }
  "number printing" should{
    "whole number pretty print" ! forAll{(n: Long) =>
      jNumberOrNull(n).nospaces must_=== "%.0f".format(n.toDouble)
    }
    "fractional number pretty print" ! forAll(arbitrary[(Double, Double)].filter{case (first, second) => second != 0}.map(pair => pair._1 / pair._2).filter(d => d != d.floor)){d =>
      jNumberOrNull(d).nospaces must_=== d.toString
    }
  }
}
