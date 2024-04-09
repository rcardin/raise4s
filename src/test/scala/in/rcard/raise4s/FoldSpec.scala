package in.rcard.raise4s

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FoldSpec extends AnyFlatSpec with Matchers {

  trait Error
  case object MyError extends Error
  
  "The fold function" should "transform the result of the given Raise function" in {
    val actual: String = fold(
      () => 42,
      throwable => "Error: " + throwable.getMessage,
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Value: 42"
  }
  
  it should "transform the result of the given Raise function when it throws an exception" in {
    val actual: String = fold(
      () => throw new RuntimeException("Boom!"),
      throwable => "Error: " + throwable.getMessage,
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Error: Boom!"
  }
  
    it should "transform the result of the given Raise function when it returns an error" in {
        val actual: String = fold(
        () => raise(MyError),
        throwable => "Error: " + throwable.getMessage,
        error => "Error: " + error,
        value => "Value: " + value
        )
    
        actual shouldBe "Error: MyError"
    }
}
