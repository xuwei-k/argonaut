package argonaut

import org.specs2._

trait ArgonautSpec extends Specification with ScalaCheck {

  implicit class MustEqualExtension[A](private val a1: A) {
    def must_==[A](a2: A) = a1 must beEqualTo(a2)
  }
}
