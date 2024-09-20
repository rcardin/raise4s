package in.rcard.raise4s.cats

import in.rcard.raise4s.Raise
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RaiseMonadErrorSpec extends AnyFlatSpec with Matchers {
  
  "raiseError" should "raise an error" in {
    val raiseMonadError = RaiseMonadError[String]()
    val actual: String | Nothing = Raise.run { raiseMonadError.raiseError("error") }
    actual shouldBe "error"
  }
  
  "pure" should "lift a value into the Raise context" in {
    val raiseMonadError = RaiseMonadError[String]()
    val actual: String | Int = Raise.run { raiseMonadError.pure(42) }
    actual shouldBe 42
  }
}
