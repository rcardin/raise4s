package in.rcard.raise4s

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.experimental

@experimental
class FoldExperimentalSpec extends AnyFlatSpec with Matchers {

  trait Error
  case object MyError extends Error

  "runRaise" should "return the value of the given Raise function" in {
    val actual: Error | Int = runRaise { 42 }

    actual shouldBe 42
  }

  it should "return the raised error" in {
    val actual: Error | Int = runRaise {
      raise(MyError)
    }

    actual shouldBe MyError
  }
}
