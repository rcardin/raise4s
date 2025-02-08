package in.rcard.raise4s

import in.rcard.raise4s.Raise.{raise, traced}
import in.rcard.raise4s.Strategies.{MapError, TraceWith, anyRaised}
import in.rcard.raise4s.Strategies.MapError.mappedRaise
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

  "MapError" should "allow raising the original error type" in {
    val finalLambda: String raises Int = {
      given MapError[String, Int] = error => error.length
      raise(42)
    }
    val result: Int | String = Raise.run(finalLambda)
    result shouldBe 42
  }

  it should "return the happy path value if no error is raised" in {
    val finalLambda: String raises Int = {
      given MapError[String, Int] = error => error.length
      "Hello"
    }
    val result: Int | String = Raise.run(finalLambda)
    result shouldBe "Hello"
  }

  "TraceWith" should "allow defining a strategy that trace the error and return it" in {
    val queue = collection.mutable.ListBuffer.empty[String]
    given TraceWith[String] = trace => {
      queue += trace.original
      trace.printStackTrace()
    }

    val lambda: Int raises String = traced {
      raise("Oops!")
    }

    val actual: String | Int = Raise.run(lambda)

    actual shouldBe "Oops!"
    queue should contain("Oops!")
  }

  it should "return the happy path value if no error is raised" in {
    val queue = collection.mutable.ListBuffer.empty[String]
    given TraceWith[String] = trace => {
      queue += trace.original
      trace.printStackTrace()
    }

    val lambda: Int raises String = traced {
      42
    }

    val actual: Int | String = Raise.run(lambda)

    actual shouldBe 42
    queue shouldBe empty
  }
}
