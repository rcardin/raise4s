package in.rcard.raise4s

import in.rcard.raise4s.Strategies.{MapError, anyRaised}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StrategiesSpec extends AnyFlatSpec with Matchers {

  "anyRaised" should "allow defining a strategy that throw any given error" in {
    val lambdaRaisingError: Int raises String = Raise.raise("Oops!")
    given anyRaised                           = error => throw new RuntimeException(error.toString)
    val ex = intercept[RuntimeException] {
      lambdaRaisingError
    }
    ex.getMessage shouldBe "Oops!"
  }

  "MapError" should "allow defining a strategy that map an error to another one" in {
    given MapError[String, Int]               = error => error.length
    val lambdaRaisingError: Int raises String = Raise.raise("Oops!")
    val finalLambda: String raises Int = {
      val r: Int = lambdaRaisingError
      r.toString
    }
    val result: Int | String = Raise.run(finalLambda)
    result shouldBe 5
  }
}
