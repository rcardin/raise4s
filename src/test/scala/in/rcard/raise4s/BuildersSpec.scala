package in.rcard.raise4s

import in.rcard.raise4s.OptionPredef.bind
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

  "Option.bind" should "return the value for a Some and raise an error for a None" in {
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None

    val actual = option {
      val x = some.bind()
      val y =  recover({ none.bind() }, { _ => 1 })
      x + y
    }

    actual should be(Some(2))

  }
}
