package in.rcard.raise4s.munit

import in.rcard.raise4s.Raise
import munit.Assertions.fail
import munit.Location

import scala.reflect.ClassTag

/** Set of assertions for tests that can raise errors.
  */
trait RaiseAssertions {

  /** Evaluates the given expression and asserts that the expected raised error is of type `T`.
    *
    * <h2>Example</h2>
    * {{{
    * val error: String = interceptR[String] {
    *   Raise.raise("An error occurred")
    * }
    * assertEquals(error, "An error occurred")
    * }}}
    *
    * @param body
    *   The function that should raise an error
    * @tparam Error
    *   The type of the error to intercept
    * @return
    *   The raised error
    */
  def interceptR[Error](
      body: Raise[Error] ?=> Any
  )(implicit ErrorClass: ClassTag[Error], loc: Location): Error = {
    val result: Error | Any = Raise.run {
      body
    }
    result match
      case error: Error => error
      case _ =>
        val expectedExceptionMsg =
          s"Expected error of type '${ErrorClass.runtimeClass.getName}' but body evaluated successfully"
        fail(expectedExceptionMsg)
  }
}
