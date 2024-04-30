package in.rcard.raise4s

import in.rcard.raise4s.Bind.value
import in.rcard.raise4s.Raise.recover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BindTest extends AnyFlatSpec with Matchers {
  "Either.value" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Either[Nothing, Int] = Right(1)
    val left: Either[String, Int] = Left("error")

    val actual = Raise.either {
      val x = one.value
      val y = recover({ left.value }) { _ => 1 }
      x + y
    }

    actual should be(Right(2))
  }

  "Option.value" should "return the value for a Some and raise an error for a None" in {
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None

    val actual = Raise.option {
      val x = some.value
      val y = recover({ none.value }) { _ => 1 }
      x + y
    }

    actual should be(Some(2))

  }
}
