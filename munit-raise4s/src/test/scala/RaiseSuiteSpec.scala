import in.rcard.raise4s.munit.RaiseSuite
import in.rcard.raise4s.{Raise, raises}
import munit.FailException

class RaiseSuiteSpec extends RaiseSuite {
  testR("A test should succeed on successful Raise instances") {
    val one: Int raises String = 1
    val two: Int raises String = 2
    assert(one < two)
  }

  testR("A test should fail on failed Raise instances".fail) {
    val one: Int raises String = Raise.raise("An error occurred")
    val two: Int raises String = 2
    assert(one < two)
  }

  testR("intercept assertion should succeed on a raised error") {
    val error: String = interceptR[String] {
      Raise.raise("An error occurred")
    }

    assertEquals(error, "An error occurred")
  }

  testR("intercept assertion should fail on a successful Raise instance") {
    val actualException = intercept[FailException] {
      interceptR[String] {
        42
      }
    }

    assertEquals(
      actualException.getMessage,
      "Expected error of type 'java.lang.String' but body evaluated successfully"
    )
  }
}
