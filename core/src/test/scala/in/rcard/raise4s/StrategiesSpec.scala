package in.rcard.raise4s

import in.rcard.raise4s.Strategies.anyRaised
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StrategiesSpec extends AnyFlatSpec with Matchers {

  "anyRaised" should "allow defining a strategy that throw any given error" in {
    val lambdaRaisingError: Int raises String = Raise.raise("Oops!")
    given anyRaised = error => throw new RuntimeException(error.toString)
    val ex = intercept[RuntimeException] {
      lambdaRaisingError
    }
    ex.getMessage shouldBe "Oops!"
  }
}
