package in.rcard.raise4s.cats

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import in.rcard.raise4s.Raise

object CatsBind {
  extension [Error, A](validated: Validated[Error, A])
    inline def value(using Raise[Error]): A = validated match {
      case Valid(value)   => value
      case Invalid(error) => Raise.raise(error)
    }

  extension [Error, A](iterable: Iterable[Raise[Error] ?=> A])
    /**
     * Accumulates the errors of executions of the elements of the iterable and returns a list of
     * the values or a non-empty list of the accumulated errors.
     *
     * <h2>Example</h2>
     * {{{
     * val iterableWithInnerRaise: List[Int raises String] =
     *   List(1, 2, 3, 4, 5).map { value =>
     *     if (value % 2 == 0) {
     *       Raise.raise(value.toString)
     *     } else {
     *       value
     *     }
     *   }
     * val iterableWithOuterRaise: List[Int] raises NonEmptyList[String] =
     *   iterableWithInnerRaise.values
     * val actual = Raise.fold(
     *   iterableWithOuterRaise,
     *   identity,
     *   identity
     * )
     * actual shouldBe NonEmptyList.of("2", "4")
     * }}}
     * 
     * @return A list of the values or a non-empty list of the accumulated errors
     */
    inline def values: RaiseNel[Error] ?=> List[A] = CatsRaise.mapOrAccumulate(iterable)(identity)
}
