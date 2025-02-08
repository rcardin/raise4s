package in.rcard.raise4s

object Strategies {

  /** An exception that make it easy to throw a logic-typed error if the error is not already a
    * [[Throwable]].
    *
    * @param error
    *   The logic-typed error to raise
    * @tparam E
    *   The type of the logic-typed error
    */
  class UnsafeRaiseException[E](val error: E) extends Exception()

  type anyRaised = Raise[Any]

  infix type mapTo[From, To] = MapError[From, To]

  /** A strategy that allows to map an error to another one. As a strategy, it should be used as a
    * `given` instance. Its behavior is comparable to the [[Raise.withError]] method.
    *
    * <h2>Example</h2>
    * {{{
    * val finalLambda: String raises Int = {
    *   given MapError[String, Int] = error => error.length
    *   raise("Oops!")
    * }
    *
    * val result: Int | String = Raise.run(finalLambda)
    * result shouldBe 5
    * }}}
    *
    * @tparam From
    *   The original error type
    * @tparam To
    *   The error type to map to
    */
  trait MapError[From, To] {

    /** Maps an error to another one.
      * @param error
      *   The error to map
      * @return
      *   The mapped error
      */
    def map(error: From): To
  }
  object MapError {
    given mappedRaise: [From, To] =>(r: Raise[To]) => (me: MapError[From, To]) => Raise[From] = (e: From) => r.raise(me.map(e))
  }
  /** A strategy that allows to recover from an error of type [Error]. As a strategy, it should be
    * used as a `given` instance. If used with the [[Raise.recoverable]] DSL, its behavior is
    * comparable to the [[Raise.recover]] method.
    *
    * <h2>Example</h2>
    * {{{
    * given RecoverWith[String, Int] = error => 43
    * val actual = Raise.recoverable {
    *   Raise.raise("error")
    * }
    * actual should be(43)
    * }}}
    *
    * @tparam Error
    *   The type of the error to recover from
    * @tparam A
    *   The type of the value to return if the recovery is successful
    *
    * @see
    *   [[Raise.recoverable]]
    */
  trait RecoverWith[Error, A] {
    def recover(error: Error): A
  }

  /** Implement the `raise` method to throw a [[Traced]] exception with the error to trace instead
    * of a [[Raised]] exception.
    */
  private[raise4s] class TracedRaise extends Raise[Any]:
    def raise(e: Any): Nothing = throw Traced(this, e)

  /** The exception that wraps the original error in case of tracing. The difference with the [[Raised]] exception is that
   * the exception contains a full stack trace.
    * @param original
    *   The original error to trace
    * @tparam Error
    *   The type of the error to trace
    */
  case class Traced[Error](raise: TracedRaise, original: Error) extends Exception

  /** A strategy that allows to trace an error and return it. The [[trace]] method represent the
    * behavior to trace the error. As a strategy, it should be used as a `given` instance. Use the
    * type class instance with the [[Raise.traced]] DSL.
    *
    * <h2>Example</h2>
    * {{{
    * given TraceWith[String] = trace => {
    *   trace.printStackTrace()
    * }
    * val lambda: Int raises String = traced {
    *   raise("Oops!")
    * }
    * val actual: String | Int = Raise.run(lambda)
    * actual shouldBe "Oops!"
    * }}}
    *
    * @tparam Error
    *   The type of the error to trace
    */
  trait TraceWith[Error] {
    def trace(traced: Traced[Error]): Unit
  }
}
