package in.rcard.raise4s.cats

import cats.Semigroup
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import in.rcard.raise4s.Raise

object CatsBind {
  extension [Error, A](validated: Validated[Error, A])
    inline def value(using Raise[Error]): A = validated match {
      case Valid(value)   => value
      case Invalid(error) => Raise.raise(error)
    }

  extension [Error, A](iterable: Iterable[Raise[Error] ?=> A])
    /** Accumulates the errors of executions of the elements of the iterable and returns a list of
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
      * @return
      *   A list of the values or a non-empty list of the accumulated errors
      */
    inline def values: RaiseNel[Error] ?=> List[A] = CatsRaise.mapOrAccumulate(iterable)(identity)

    /**
     * Accumulates all the occurred errors using the combine operator of the implicit [[Semigroup]]  and returns 
     * the list of the values or the accumulated errors.
     *
     * <h2>Example</h2>
     * {{{
     * import cats.data.*
     * import in.rcard.raise4s.cats.CatsBind.combineErrorsS
     * 
     * val iterableWithInnerRaise: List[Int raises String] =
     *   List(1, 2, 3, 4, 5).map { value =>
     *     if (value % 2 == 0) {
     *       Raise.raise(value.toString)
     *     } else {
     *       value
     *     }
     *   }
     *   
     * val iterableWithOuterRaise: List[Int] raises String = iterableWithInnerRaise.combineErrorsS
     * 
     * val actual = Raise.fold(
     *   iterableWithOuterRaise,
     *   identity,
     *   identity
     * )
     * 
     * actual shouldBe "24"
     * }}}
     *
     * @param semigroup The semigroup to combine the errors defined on the type `Error`
     * @return The list of the values or the accumulated error
     */
    inline def combineErrorsS(using semigroup: Semigroup[Error]): Raise[Error] ?=> List[A] =
      CatsRaise.mapOrAccumulateS(iterable)(identity)

  extension [Error, A](nonEmptyList: NonEmptyList[Raise[Error] ?=> A])
    /** Accumulates the errors of executions of the elements of the non-empty list and returns a
      * non-empty list of the values or a non-empty list of the accumulated errors.
      *
      * <h2>Example</h2>
      * {{{
      * val nonEmptyListWithInnerRaise: NonEmptyList[Int raises String] =
      *   NonEmptyList.of(1, 2, 3, 4, 5).map { value =>
      *     if (value % 2 == 0) {
      *       Raise.raise(value.toString)
      *     } else {
      *       value
      *     }
      *   }
      *
      * val nonEmptyWithOuterRaise: NonEmptyList[Int] raises NonEmptyList[String] =
      *   nonEmptyListWithInnerRaise.values
      *
      * val actual = Raise.fold(
      *   nonEmptyWithOuterRaise,
      *   identity,
      *   identity
      * )
      *
      * actual shouldBe NonEmptyList.of("2", "4")
      * }}}
      *
      * @return
      *   A non-empty list of the values or a non-empty list of the accumulated errors
      */
    inline def values: RaiseNel[Error] ?=> NonEmptyList[A] =
      CatsRaise.mapOrAccumulate(nonEmptyList)(identity)

    /**
     * Accumulates all the occurred errors using the combine operator of the implicit [[Semigroup]]  and returns
     * the non-empty list of the values or the accumulated errors.
     *
     * <h2>Example</h2>
     * {{{
     * val iterableWithInnerRaise: NonEmptyList[Int raises String] =
     *   NonEmptyList.of(1, 2, 3, 4, 5).map { value =>
     *     if (value % 2 == 0) {
     *       Raise.raise(value.toString)
     *     } else {
     *       value
     *     }
     *   }
     * val iterableWithOuterRaise: NonEmptyList[Int] raises String =
     *   iterableWithInnerRaise.combineErrorsS
     * val actual = Raise.fold(
     *   iterableWithOuterRaise,
     *   identity,
     *   identity
     * )
     * actual shouldBe "24"
     * }}}
     *
     * @param semigroup The semigroup to combine the errors defined on the type `Error`
     * @return The non-empty list of the values or the accumulated error
     */
    inline def combineErrorsS(using semigroup: Semigroup[Error]): Raise[Error] ?=> NonEmptyList[A] =
      CatsRaise.mapOrAccumulateS(nonEmptyList)(identity)
}
