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

  /** A strategy that allow to map an error to another one. As a strategy, it should be used as a
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
  trait MapError[From, To] extends Raise[From] {

    /** Maps an error to another one.
      * @param error
      *   The error to map
      * @return
      *   The mapped error
      */
    def map(error: From): To

    def raise(error: From): Nothing = throw Raised(map(error))
  }
}
