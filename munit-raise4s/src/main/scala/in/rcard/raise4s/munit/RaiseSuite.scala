package in.rcard.raise4s.munit

import in.rcard.raise4s.Raise
import munit.{FunSuite, Location, TestOptions}

/** Suite that provides a test methods that can handle computations that can raise errors.
  */
abstract class RaiseSuite extends FunSuite with RaiseAssertions {

  /** A test for computations that can raise errors. <h2>Example</h2>
    * {{{
    * testR("A test should succeed on successful Raise instances") {
    *   val one: Int raises String = 1
    *   val two: Int raises String = 2
    *   assert(one < two)
    * }
    * }}}
    *
    * @tparam Error
    *   The type of the error that can be raised
    */
  def testR[Error](name: String)(body: Raise[Error] ?=> Unit)(implicit loc: Location): Unit = {
    test(new TestOptions(name))(
      Raise.fold(
        block = body,
        recover = error =>
          fail(s"Expected the test not to raise any errors but it did with error '$error'"),
        transform = identity
      )
    )
  }

  /** A test for computations that can raise errors and that can be configured with the given
    * options. <h2>Example</h2>
    * {{{
    * testR("A test should fail on failed Raise instances".fail) {
    *   val one: Int raises String = Raise.raise("An error occurred")
    *   val two: Int raises String = 2
    *   assert(one < two)
    * }
    * }}}
    *
    * @tparam Error
    *   The type of the error that can be raised
    */
  def testR[Error](
      options: TestOptions
  )(body: Raise[Error] ?=> Unit)(implicit loc: Location): Unit = {
    test(options)(
      Raise.fold(
        block = body,
        recover = error =>
          fail(s"Expected the test not to raise any errors but it did with error '$error'"),
        transform = identity
      )
    )
  }
}
