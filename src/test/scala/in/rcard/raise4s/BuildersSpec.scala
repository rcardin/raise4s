package in.rcard.raise4s

import in.rcard.raise4s.RaiseEitherPredef.bind
import in.rcard.raise4s.RaiseOptionPredef.bind
import in.rcard.raise4s.RaiseTryPredef.bind
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class BuildersSpec extends AnyFlatSpec with Matchers {

  "The either builder" should "create a Left instance" in {
    either { raise("error") } should be(Left("error"))
  }

  it should "create a Right instance" in {
    either { "success" } should be(Right("success"))
  }

  "Either.bind" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Either[Nothing, Int] = Right(1)
    val left: Either[String, Int] = Left("error")

    val actual = either {
      val x = one.bind()
      val y = recover(
        {
          left.bind()
        },
        { _ => 1 }
      )
      x + y
    }

    actual should be(Right(2))
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
      val y = recover({ none.bind() }, { _ => 1 })
      x + y
    }

    actual should be(Some(2))

  }

  "The try builder" should "create a Failure instance" in {
    val actual = $try { raise(new Exception("error")) }

    actual.isFailure should be(true)
  }

  it should "create a Success instance" in {
    val actual = $try { "success" }

    actual should be(Success("success"))
  }

  "Try.bind" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Try[Int]     = Success(1)
    val failure: Try[Int] = Failure(new Exception("error"))

    val actual = $try {
      val x = one.bind()
      val y = recover({ failure.bind() }, { _ => 1 })
      x + y
    }

    actual should be(Success(2))
  }
}
