package in.rcard.raise4s

import in.rcard.raise4s.RaiseAnyPredef.{succeed, raise}
import in.rcard.raise4s.RaiseEitherPredef.bind
import in.rcard.raise4s.RaiseOptionPredef.bind
import in.rcard.raise4s.RaiseTryPredef.bind
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class BuildersSpec extends AnyFlatSpec with Matchers {

  "The either builder" should "create a Left instance" in {
    Raise.either { Raise.raise("error") } should be(Left("error"))
  }

  it should "create a Right instance" in {
    Raise.either { "success" } should be(Right("success"))
  }

  "Either.bind" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Either[Nothing, Int] = Right(1)
    val left: Either[String, Int] = Left("error")

    val actual = Raise.either {
      val x = one.bind()
      val y = Raise.recover(
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
    Raise.option { Raise.raise(None) } should be(None)
  }

  it should "create a Some instance" in {
    Raise.option { "success" } should be(Some("success"))
  }

  "Option.bind" should "return the value for a Some and raise an error for a None" in {
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None

    val actual = Raise.option {
      val x = some.bind()
      val y = Raise.recover({ none.bind() }, { _ => 1 })
      x + y
    }

    actual should be(Some(2))

  }

  "The try builder" should "create a Failure instance" in {
    val actual = Raise.$try { Raise.raise(new Exception("error")) }

    actual.isFailure should be(true)
  }

  it should "create a Success instance" in {
    val actual = Raise.$try { "success" }

    actual should be(Success("success"))
  }

  "Try.bind" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Try[Int]     = Success(1)
    val failure: Try[Int] = Failure(new Exception("error"))

    val actual = Raise.$try {
      val x = one.bind()
      val y = Raise.recover({ failure.bind() }, { _ => 1 })
      x + y
    }

    actual should be(Success(2))
  }

  "The succeed extension method" should "lift a value in the raise context" in {
    Raise.fold(
      42.succeed,
      identity,
      _ => "meaning of life"
    ) should be("meaning of life")
  }

  it should "be composable with any other raising lambda" in {
    Raise.fold(
      {
        val meaningOfLife: Int = 42.succeed
        Raise.raise("error")
      },
      identity,
      _ => "success"
    ) should be("error")
  }

  "The raise builder" should "raise an error using the extension receiver value" in {
    Raise.fold(
      "error".raise[Int],
      identity,
      _ => "42"
    ) should be("error")
  }
}
