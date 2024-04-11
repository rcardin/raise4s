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
}
