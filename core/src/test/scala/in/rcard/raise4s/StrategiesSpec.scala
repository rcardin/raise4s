package in.rcard.raise4s

import in.rcard.raise4s.Raise.raise
import in.rcard.raise4s.Strategies.{MapError, anyRaised}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StrategiesSpec extends AnyFlatSpec with Matchers {

  "anyRaised" should "allow defining a strategy that throw any given error" in {
    val lambdaRaisingError: Int raises String = raise("Oops!")
    given anyRaised                           = error => throw new RuntimeException(error.toString)
    val ex = intercept[RuntimeException] {
      lambdaRaisingError
    }
    ex.getMessage shouldBe "Oops!"
  }

  it should "return the happy path value if no error is raised" in {
    val lambdaRaisingError: String raises Int = "Hello"
    given anyRaised                           = error => throw new RuntimeException(error.toString)
    lambdaRaisingError shouldBe "Hello"
  }

  "MapError" should "allow defining a strategy that map an error to another one" in {
    val finalLambda: String raises Int = {
      given MapError[String, Int] = error => error.length
      raise("Oops!")
    }
    val result: Int | String = Raise.run(finalLambda)
    result shouldBe 5
  }

  it should "return the happy path value if no error is raised" in {
    val finalLambda: String raises Int = {
      given MapError[String, Int] = error => error.length
      "Hello"
    }
    val result: Int | String = Raise.run(finalLambda)
    result shouldBe "Hello"
  }
}
