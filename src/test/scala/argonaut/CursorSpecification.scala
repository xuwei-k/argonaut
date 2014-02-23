package argonaut

import org.scalacheck.Prop._
import org.scalacheck.Properties
import Data._
import Argonaut._
import scalaz._
import std.AllInstances._, syntax.equal._

object CursorSpecification extends SpecLite {

  "Cursor" should {
    "Json->Cursor->Json" ! forAll((j: Json) =>
      (-(+j)) must_=== j)
    "Json->Cursor->focus" ! forAll((j: Json) =>
      (+j).focus must_=== j)
    "withFocus on focus changes nothing" ! forAll((c: Cursor) =>
      c.withFocus(_ => c.focus) must_=== c)
    "withFocus identity changes nothing" ! forAll((c: Cursor) =>
      c.withFocus(j => j) must_=== c)
    ">-> aliases withFocus" ! forAll((c: Cursor, f: Json => Json) =>
      (c withFocus f) must_=== (c >-> f))
    "set gives focus" ! forAll((c: Cursor, j: Json) =>
      (c set j).focus must_=== j)
    ":= aliases set" ! forAll((c: Cursor, j: Json) =>
      (c set j) must_=== (c := j))
    "lefts head is left" ! forAll((c: Cursor) =>
      c.lefts foreach(
        _.headOption must_=== c.left.map(_.focus)
      ))
    "rights head is right" ! forAll((c: Cursor) =>
      c.rights foreach(
        _.headOption must_=== c.right.map(_.focus)
      ))
    "first has no lefts" ! forAll((c: Cursor) =>
      c.first forall (_.left.isEmpty))
    "last has no rights" ! forAll((c: Cursor) =>
      c.last forall (_.right.isEmpty))
    "left->right" ! forAll((c: Cursor) =>
      c.left forall (_.right exists (_ === c)))
    "right->left" ! forAll((c: Cursor) =>
      c.right forall (_.left exists (_ === c)))
    "downArray->up" ! forAll((c: Cursor) =>
      c.downArray forall (_.up exists (_ === c)))
    "downArray" ! forAll((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.map(_.focus) must_=== Some(x))
    "downAt constant" ! forAll((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downAt(_ => true).map(_.focus) must_=== Some(x))
    "downAt constant true is same as down array" ! forAll((xs: List[Json]) =>
      jArray(xs).cursor.downAt(_ => true).map(_.focus) must_=== jArray(xs).cursor.downArray.map(_.focus))
    "downAt" ! forAll((ys: List[Json], x: Json, xs: List[Json]) =>
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).map(_.focus) must_=== Some(x))
    "first" ! forAll((y: Json, ys: List[Json], x: Json, xs: List[Json]) =>
      jArray((y :: ys) ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.first).map(_.focus) must_=== Some(y))
    "last" ! forAll((ys: List[Json], x: Json, xs: List[Json], z: Json) =>
      jArray(ys ::: (x :: xs) ::: List(z)).cursor.downAt(_ == x).flatMap(_.last).map(_.focus) must_=== Some(z))
    "rightAt" ! forAll((xx: Json, x: Json, xs: List[Json]) =>
      jArray(xx :: x :: xs).cursor.downArray.flatMap(_.rightAt(_ => true)).map(_.focus) must_===  Some(x))
    "right" ! forAll((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.flatMap(_.right).map(_.focus) must_=== xs.headOption)
    "right is same as rightN(1)" ! forAll((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.flatMap(_.right).map(_.focus) must_==
       jArray(x :: xs).cursor.downArray.flatMap(_.rightN(1)).map(_.focus))
    "rightN(0) is a no op" ! forAll((ys: List[Json], x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.map(_.focus) must_==
       jArray(x :: xs).cursor.downArray.flatMap(_.rightN(0)).map(_.focus))
    "leftN" ! forAll((ys: List[Json], x: Json, xs: List[Json]) => !ys.contains(x) ==> {
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.leftN(Math.max(ys.size, 1))).map(_.focus) must_===  ys.headOption })
    "rightN" ! forAll((ys: List[Json], x: Json, xs: List[Json]) => !ys.contains(x) ==> {
      jArray(ys ::: (x :: xs)).cursor.downAt(_ == x).flatMap(_.rightN(Math.max(xs.size, 1))).map(_.focus) must_===  xs.lastOption })
    "find" ! forAll((x: Json, xs: List[Json]) =>
      jArray(x :: xs).cursor.downArray.flatMap(_.find(_ => true)).map(_.focus) must_=== Some(x))
  }

}
