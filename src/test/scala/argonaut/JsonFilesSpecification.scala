package argonaut

import org.scalacheck._
import scalaz._, std.option._
import Argonaut._
import java.io.File

object JsonFilesSpecification extends SpecLite {
  def find = new File(getClass.getResource("/data").toURI).listFiles.toList

  case class JsonFile(file: File)

  implicit def JsonFileArbitrary: Arbitrary[JsonFile] =
    Arbitrary(Gen.oneOf(find.map(JsonFile)))

  property("can print and get same result") = Prop.forAll{
    jsonfile: JsonFile => {
      val string = scala.io.Source.fromFile(jsonfile.file).mkString
      val parsed = string.parseOption
      val json = parsed.getOrElse(sys.error("could not parse json file [" + jsonfile + "]"))
      json.nospaces.parseOption must_=== Some(json)
      json.spaces2.parseOption must_=== Some(json)
      json.spaces4.parseOption must_=== Some(json)
    }
  }
}
