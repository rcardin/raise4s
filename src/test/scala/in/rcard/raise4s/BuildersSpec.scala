package in.rcard.raise4s

import in.rcard.raise4s.RaiseAnyPredef.{raise, succeed}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class BuildersSpec extends AnyFlatSpec with Matchers {

  "The either builder" should "create a Left instance" in {
    Raise.either { Raise.raise("error") } should be(Left("error"))
  }

  it should "create a Right instance" in {
    Raise.either { "success" } should be(Right("success"))
  }

  "The option builder" should "create a None instance" in {
    Raise.option { Raise.raise(None) } should be(None)
  }

  it should "create a Some instance" in {
    Raise.option { "success" } should be(Some("success"))
  }

  "The try builder" should "create a Failure instance" in {
    val actual = Raise.asTry { Raise.raise(new Exception("error")) }

    actual.isFailure should be(true)
  }

  it should "create a Success instance" in {
    val actual = Raise.asTry { "success" }

    actual should be(Success("success"))
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
