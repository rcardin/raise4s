package in.rcard.raise4s

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildersSpec extends AnyFlatSpec with Matchers {

  "The either builder" should "create a Left instance" in {
    either { raise("error") } should be(Left("error"))
  }

  it should "create a Right instance" in {
    either { "success" } should be(Right("success"))
  }

  "The option builder" should "create a None instance" in {
    option { raise(None) } should be(None)
  }

  it should "create a Some instance" in {
    option { "success" } should be(Some("success"))
  }
}
