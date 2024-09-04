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

  infix type mapTo[EO, EN] = MapError[EO, EN]
  trait MapError[EO, EN] extends Raise[EO] {
    def map(error: EO): EN

    def raise(error: EO): Nothing = throw Raised(map(error))
  }
}
