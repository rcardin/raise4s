package in.rcard.raise4s

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RuntimeSpec extends AnyFlatSpec with Matchers {
  "Raise.run" should "return the happy path value is no error was risen" in {
    val happyPathBlock: Int raises String = 42
    val result: String | Int = Runtime._run(happyPathBlock)
    result should be(42)
  }

  it should "return the error value if an error was risen" in {
    val errorBlock: Int raises String = Raise.raise("error")
    val result: String | Int = Runtime._run(errorBlock)
    result should be("error")
  }

  it should "be transparent to any thrown exception" in {
    val exceptionBlock: Int raises String = throw new RuntimeException("error")
    assertThrows[RuntimeException] {
      Runtime._run(exceptionBlock)
    }
  }
}
