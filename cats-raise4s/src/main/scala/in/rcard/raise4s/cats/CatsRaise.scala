package in.rcard.raise4s.cats

import cats.Semigroup
import cats.data.*
import in.rcard.raise4s.Raise

type RaiseNel[Error] = Raise[NonEmptyList[Error]]

object CatsRaise {

  /** Transform every element of `iterable` using the given `transform`, or accumulate all the
    * occurred errors using the [[Semigroup]] type class defined on the `Error` type. The tailing
    * `S` in the name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.mapOrAccumulateS(List(1, 2, 3, 4, 5)) { value1 =>
    *     value1 + 1
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual shouldBe List(2, 3, 4, 5, 6)
    * }}}
    *
    * @param iterable
    *   The collection of elements to transform
    * @param transform
    *   The transformation to apply to each element that can raise an error of type `Error`
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised. It must have a [[Semigroup]] instance
    *   available
    * @tparam A
    *   The type of the elements in the `iterable`
    * @tparam B
    *   The type of the transformed elements
    * @return
    *   A list of transformed elements
    */
  inline def mapOrAccumulateS[Error: Semigroup, A, B](iterable: Iterable[A])(
      inline transform: Raise[Error] ?=> A => B
  )(using r: Raise[Error]): List[B] =
    Raise.mapOrAccumulate(iterable, Semigroup[Error].combine)(transform)

  /** Transform every element of `nonEmptyList` using the given `transform`, or accumulate all the
    * occurred errors using the [[Semigroup]] type class defined on the `Error` type. The tailing
    * `S` in the name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * val block: NonEmptyList[Int] raises MyError2 =
    *   CatsRaise.mapOrAccumulateS(NonEmptyList.of(1, 2, 3, 4, 5)) { value =>
    *     if (value % 2 == 0) {
    *       Raise.raise(MyError2(List(value.toString)))
    *     } else {
    *       value
    *     }
    *   }
    * val actual = Raise.fold(
    *   block,
    *   identity,
    *   identity
    * )
    * actual shouldBe MyError2(List("2", "4"))
    * }}}
    *
    * @param nonEmptyList
    *   The non-empty list of elements to transform
    * @param transform
    *   The transformation to apply to each element that can raise an error of type `Error`
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised. It must have a [[Semigroup]] instance
    *   available
    * @tparam A
    *   The type of the elements in the original non-empty list
    * @tparam B
    *   The type of the transformed elements
    * @return
    *   A non-empty list of transformed elements
    */
  inline def mapOrAccumulateS[Error: Semigroup, A, B](nonEmptyList: NonEmptyList[A])(
      inline transform: Raise[Error] ?=> A => B
  )(using r: Raise[Error]): NonEmptyList[B] = {
    val result = Raise.mapOrAccumulate(nonEmptyList.toList, Semigroup[Error].combine)(transform)
    // It's safe to call get here because we started from a non-empty list
    NonEmptyList.fromList(result).get
  }

  /** Accumulate the errors obtained by executing the `transform` over every element of `iterable`.
    * The error channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = Raise.mapOrAccumulate(List(1, 2, 3, 4, 5)) {
    *   _ + 1
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual shouldBe List(2, 3, 4, 5, 6)
    * }}}
    *
    * @param iterable
    *   The collection of elements to transform
    * @param transform
    *   The transformation to apply to each element that can raise an error of type `Error`
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised
    * @tparam A
    *   The type of the elements in the `iterable`
    * @tparam B
    *   The type of the transformed elements
    * @return
    *   A list of transformed elements
    */
  inline def mapOrAccumulate[Error, A, B](iterable: Iterable[A])(
      inline transform: Raise[Error] ?=> A => B
  )(using r: RaiseNel[Error]): List[B] =
    val errors  = collection.mutable.ArrayBuffer.empty[Error]
    val results = collection.mutable.ArrayBuffer.empty[B]
    iterable.foreach(a =>
      Raise.fold(
        transform(a),
        error => errors += error,
        result => results += result
      )
    )
    NonEmptyList.fromList(errors.toList).fold(results.toList)(r.raise)

  /** Accumulate the errors obtained by executing the `transform` over every element of `iterable`.
    * The error channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: NonEmptyList[Int] raises NonEmptyList[String] =
    *   CatsRaise.mapOrAccumulate(NonEmptyList.of(1, 2, 3, 4, 5)) { value =>
    *     if (value % 2 == 0) {
    *       Raise.raise(value.toString)
    *     } else {
    *       value
    *     }
    *   }
    * val actual = Raise.fold(
    *   block,
    *   identity,
    *   identity
    * )
    * actual shouldBe NonEmptyList.of("2", "4")
    * }}}
    *
    * @param nonEmptyList
    *   The non-empty list of elements to transform
    * @param transform
    *   The transformation to apply to each element that can raise an error of type `Error`
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised
    * @tparam A
    *   The type of the elements in the `iterable`
    * @tparam B
    *   The type of the transformed elements
    * @return
    *   A non-empty list of transformed elements
    */
  inline def mapOrAccumulate[Error, A, B](nonEmptyList: NonEmptyList[A])(
      inline transform: Raise[Error] ?=> A => B
  )(using r: RaiseNel[Error]): NonEmptyList[B] =
    val resultAsList = CatsRaise.mapOrAccumulate(nonEmptyList.toList)(transform)
    // We know it's safe to call get here because we started from a non-empty list
    NonEmptyList.fromList(resultAsList).get

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, `action8`, and `action9`, or accumulate all the occurred errors using
    * the [[Semigroup]] type class defined on the `Error` type. The tailing `S` in the name of the
    * function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }, { 5 }, { 6 }, { 7 }, { 8 }, { 9 }) {
    *     case (a, b, c, d, e, f, g, h, i) =>
    *       List(a, b, c, d, e, f, g, h, i)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param action7
    *   Code block to run on type `G`
    * @param action8
    *   Code block to run on type `H`
    * @param action9
    *   Code block to run on type `I`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the seventh code block
    * @tparam H
    *   The type of the result of the eighth code block
    * @tparam I
    *   The type of the result of the ninth code block
    * @tparam J
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D, E, F, G, H, I, J](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G,
      inline action8: Raise[Error] ?=> H,
      inline action9: Raise[Error] ?=> I
  )(inline block: (A, B, C, D, E, F, G, H, I) => J)(using r: Raise[Error]): J =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      action9
    )(block)

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, and `action8`, or accumulate all the occurred errors using the
    * [[Semigroup]] type class defined on the `Error` type. The tailing `S` in the name of the
    * function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }, { 5 }, { 6 }, { 7 }, { 8 }) {
    *     case (a, b, c, d, e, f, g, h) =>
    *       List(a, b, c, d, e, f, g, h)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param action7
    *   Code block to run on type `G`
    * @param action8
    *   Code block to run on type `H`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the seventh code block
    * @tparam H
    *   The type of the result of the eighth code block
    * @tparam I
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D, E, F, G, H, I](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G,
      inline action8: Raise[Error] ?=> H
  )(inline block: (A, B, C, D, E, F, G, H) => I)(using r: Raise[Error]): I =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      {}
    ) { (a, b, c, d, e, f, g, h, _) =>
      block(a, b, c, d, e, f, g, h)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, and `action7`, or accumulate all the occurred errors using the [[Semigroup]] type
    * class defined on the `Error` type. The tailing `S` in the name of the function stands for
    * <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }, { 5 }, { 6 }, { 7 }) {
    *     case (a, b, c, d, e, f, g) =>
    *       List(a, b, c, d, e, f, g)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6, 7))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param action7
    *   Code block to run on type `G`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the seventh code block
    * @tparam H
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D, E, F, G, H](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G
  )(inline block: (A, B, C, D, E, F, G) => H)(using r: Raise[Error]): H =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      {},
      {}
    ) { (a, b, c, d, e, f, g, _, _) =>
      block(a, b, c, d, e, f, g)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`, and
    * `action6`, or accumulate all the occurred errors using the [[Semigroup]] type class defined on
    * the `Error` type. The tailing `S` in the name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }, { 5 }, { 6 }) {
    *     case (a, b, c, d, e, f) =>
    *       List(a, b, c, d, e, f)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D, E, F, G](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F
  )(inline block: (A, B, C, D, E, F) => G)(using r: Raise[Error]): G =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      {},
      {},
      {}
    ) { (a, b, c, d, e, f, _, _, _) =>
      block(a, b, c, d, e, f)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, and `action5`,
    * or accumulate all the occurred errors using the [[Semigroup]] type class defined on the
    * `Error` type. The tailing `S` in the name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }, { 5 }) {
    *     case (a, b, c, d, e) =>
    *       List(a, b, c, d, e)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D, E, F](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E
  )(inline block: (A, B, C, D, E) => F)(using r: Raise[Error]): F =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      {},
      {},
      {},
      {}
    ) { (a, b, c, d, e, _, _, _, _) =>
      block(a, b, c, d, e)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, and `action4`, or
    * accumulate all the occurred errors using the [[Semigroup]] type class defined on the `Error`
    * type. The tailing `S` in the name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }) {
    *     case (a, b, c, d) =>
    *       List(a, b, c, d)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D, E](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D
  )(inline block: (A, B, C, D) => E)(using r: Raise[Error]): E =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      action4,
      {},
      {},
      {},
      {},
      {}
    ) { (a, b, c, d, _, _, _, _, _) =>
      block(a, b, c, d)
    }

  /** Accumulate the errors from running `action1`, `action2`, and `action3`, or accumulate all the
    * occurred errors using the [[Semigroup]] type class defined on the `Error` type. The tailing
    * `S` in the name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }) {
    *     case (a, b, c) =>
    *       List(a, b, c)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C, D](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C
  )(inline block: (A, B, C) => D)(using r: Raise[Error]): D =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      action3,
      {},
      {},
      {},
      {},
      {},
      {}
    ) { (a, b, c, _, _, _, _, _, _) =>
      block(a, b, c)
    }

  /** Accumulate the errors from running `action1`, and `action2`, or accumulate all the occurred
    * errors using the [[Semigroup]] type class defined on the `Error` type. The tailing `S` in the
    * name of the function stands for <em>Semigroup</em>.
    *
    * <h2>Example</h2>
    * {{{
    * case class MyError2(errors: List[String])
    *
    * given Semigroup[MyError2] with {
    *   def combine(error1: MyError2, error2: MyError2): MyError2 =
    *     MyError2(error1.errors ++ error2.errors)
    * }
    *
    * val block: List[Int] raises MyError2 =
    *   CatsRaise.zipOrAccumulateS({ 1 }, { 2 }) {
    *     case (a, b) =>
    *       List(a, b)
    *   }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block. It must have a
    *   [[Semigroup]] instance available
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulateS[Error: Semigroup, A, B, C](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B
  )(inline block: (A, B) => C)(using r: Raise[Error]): C =
    Raise.zipOrAccumulate(Semigroup[Error].combine)(
      action1,
      action2,
      {},
      {},
      {},
      {},
      {},
      {},
      {}
    ) { (a, b, _, _, _, _, _, _, _) =>
      block(a, b)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, `action8`, and `action9`. The error channel uses a [[NonEmptyList]]
    * instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 },
    *   { 5 },
    *   { 6 },
    *   { 7 },
    *   { 8 },
    *   { 9 }
    * ) { case (a, b, c, d, e, f, g, h, i) =>
    *   List(a, b, c, d, e, f, g, h, i)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param action7
    *   Code block to run on type `G`
    * @param action8
    *   Code block to run on type `H`
    * @param action9
    *   Code block to run on type `I`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the seventh code block
    * @tparam H
    *   The type of the result of the eighth code block
    * @tparam I
    *   The type of the result of the ninth code block
    * @tparam J
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D, E, F, G, H, I, J](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G,
      inline action8: Raise[Error] ?=> H,
      inline action9: Raise[Error] ?=> I
  )(inline block: (A, B, C, D, E, F, G, H, I) => J)(using r: RaiseNel[Error]): J = {
    val errors = collection.mutable.ArrayBuffer.empty[Error]
    val a: A = Raise.recover(action1) { newError =>
      errors += newError; null.asInstanceOf[A]
    }
    val b: B = Raise.recover(action2) { newError =>
      errors += newError; null.asInstanceOf[B]
    }
    val c: C = Raise.recover(action3) { newError =>
      errors += newError; null.asInstanceOf[C]
    }
    val d: D = Raise.recover(action4) { newError =>
      errors += newError; null.asInstanceOf[D]
    }
    val e: E = Raise.recover(action5) { newError =>
      errors += newError; null.asInstanceOf[E]
    }
    val f: F = Raise.recover(action6) { newError =>
      errors += newError; null.asInstanceOf[F]
    }
    val g: G = Raise.recover(action7) { newError =>
      errors += newError; null.asInstanceOf[G]
    }
    val h: H = Raise.recover(action8) { newError =>
      errors += newError; null.asInstanceOf[H]
    }
    val i: I = Raise.recover(action9) { newError =>
      errors += newError; null.asInstanceOf[I]
    }

    NonEmptyList.fromList(errors.toList).fold(block(a, b, c, d, e, f, g, h, i))(r.raise)
  }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, and `action8`. The error channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 },
    *   { 5 },
    *   { 6 },
    *   { 7 },
    *   { 8 }
    * ) { case (a, b, c, d, e, f, g, h) =>
    *   List(a, b, c, d, e, f, g, h)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param action7
    *   Code block to run on type `G`
    * @param action8
    *   Code block to run on type `H`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the seventh code block
    * @tparam H
    *   The type of the result of the eighth code block
    * @tparam I
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D, E, F, G, H, I](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G,
      inline action8: Raise[Error] ?=> H
  )(inline block: (A, B, C, D, E, F, G, H) => I)(using r: RaiseNel[Error]): I = {
    zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      {}
    ) { case (a, b, c, d, e, f, g, h, _) =>
      block(a, b, c, d, e, f, g, h)
    }
  }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, and `action7`. The error channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 },
    *   { 5 },
    *   { 6 },
    *   { 7 }
    * ) { case (a, b, c, d, e, f, g) =>
    *   List(a, b, c, d, e, f, g)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6, 7))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param action7
    *   Code block to run on type `G`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the seventh code block
    * @tparam H
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D, E, F, G, H](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G
  )(inline block: (A, B, C, D, E, F, G) => H)(using r: RaiseNel[Error]): H = {
    zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      {},
      {}
    ) { case (a, b, c, d, e, f, g, _, _) =>
      block(a, b, c, d, e, f, g)
    }
  }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`, and
    * `action6`. The error channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 },
    *   { 5 },
    *   { 6 }
    * ) { case (a, b, c, d, e, f) =>
    *   List(a, b, c, d, e, f)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5, 6))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param action6
    *   Code block to run on type `F`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the sixth code block
    * @tparam G
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D, E, F, G](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F
  )(inline block: (A, B, C, D, E, F) => G)(using r: RaiseNel[Error]): G = {
    zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      {},
      {},
      {}
    ) { case (a, b, c, d, e, f, _, _, _) =>
      block(a, b, c, d, e, f)
    }
  }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, and `action5`.
    * The error channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 },
    *   { 5 }
    * ) { case (a, b, c, d, e) =>
    *   List(a, b, c, d, e)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4, 5))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param action5
    *   Code block to run on type `E`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the fifth code block
    * @tparam F
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D, E, F](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E
  )(inline block: (A, B, C, D, E) => F)(using r: RaiseNel[Error]): F = {
    zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      action5,
      {},
      {},
      {},
      {}
    ) { case (a, b, c, d, e, _, _, _, _) =>
      block(a, b, c, d, e)
    }
  }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, and `action4`. The error
    * channel uses a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 }
    * ) { case (a, b, c, d) =>
    *   List(a, b, c, d)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3, 4))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param action4
    *   Code block to run on type `D`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the fourth code block
    * @tparam E
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D, E](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D
  )(inline block: (A, B, C, D) => E)(using r: RaiseNel[Error]): E = {
    zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      {},
      {},
      {},
      {},
      {}
    ) { case (a, b, c, d, _, _, _, _, _) =>
      block(a, b, c, d)
    }
  }

  /** Accumulate the errors from running `action1`, `action2`, and `action3`. The error channel uses
    * a [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 }
    * ) { case (a, b, c) =>
    *   List(a, b, c)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2, 3))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param action3
    *   Code block to run on type `C`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the third code block
    * @tparam D
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C, D](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C
  )(inline block: (A, B, C) => D)(using r: RaiseNel[Error]): D = {
    zipOrAccumulate(
      action1,
      action2,
      action3,
      {},
      {},
      {},
      {},
      {},
      {}
    ) { case (a, b, c, _, _, _, _, _, _) =>
      block(a, b, c)
    }
  }

  /** Accumulate the errors from running `action1`, and `action2`. The error channel uses a
    * [[NonEmptyList]] instance.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 },
    *   { 4 }
    * ) { case (a, b) =>
    *   List(a, b)
    * }
    * val actual = Raise.fold(
    *   block,
    *   error => fail(s"An error occurred: $error"),
    *   identity
    * )
    * actual should be(List(1, 2))
    * }}}
    *
    * @param action1
    *   Code block to run on type `A`
    * @param action2
    *   Code block to run on type `B`
    * @param block
    *   Function to run on the results of the code blocks
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error that can be raised by any code block
    * @tparam A
    *   The type of the result of the first code block
    * @tparam B
    *   The type of the result of the second code block
    * @tparam C
    *   The type of the result of the block function
    * @return
    *   The result of the block function
    */
  inline def zipOrAccumulate[Error, A, B, C](
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B
  )(inline block: (A, B) => C)(using r: RaiseNel[Error]): C = {
    zipOrAccumulate(
      action1,
      action2,
      {},
      {},
      {},
      {},
      {},
      {},
      {}
    ) { case (a, b, _, _, _, _, _, _, _) =>
      block(a, b)
    }
  }

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[Validated]].
    *   - [[Validated.Valid]] represents success,
    *   - [[Validated.Invalid]] represents logical failure.
    *
    * This function re-throws any exceptions thrown within the [[Raise]] block.
    *
    * <h2>Example</h2>
    * {{{
    * CatsRaise.validated { raise("error") } should be(Validated.invalid("error"))
    * }}}
    *
    * @param block
    *   A computation that can raise errors of type `Error`
    * @tparam A
    *   The type of the value returned by the computation
    * @tparam Error
    *   The type of the logical error that can be raised by the computation
    * @return
    *   An [[Validated]] representing the outcome of the computation
    */
  inline def validated[Error, A](inline block: Raise[Error] ?=> A): Validated[Error, A] =
    Raise.fold(
      block,
      error => Validated.invalid(error),
      value => Validated.valid(value)
    )

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[ValidatedNec]].
    *
    * <h2>Example</h2>
    * {{{
    * CatsRaise.validatedNec {
    *   raise("error")
    * } should be(Validated.invalid(NonEmptyChain.one("error")))
    * }}}
    *
    * @param block
    *   A computation that can raise errors of type `Error`
    * @tparam A
    *   The type of the value returned by the computation
    * @tparam Error
    *   The type of the logical error that can be raised by the computation
    * @return
    *   An [[ValidatedNec]] representing the outcome of the computation
    */
  inline def validatedNec[Error, A](inline block: Raise[Error] ?=> A): ValidatedNec[Error, A] =
    validated(
      Raise.withError[NonEmptyChain[Error], Error, A](error => NonEmptyChain.one(error))(block)
    )

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[ValidatedNel]].
    *
    * <h2>Example</h2>
    * {{{
    * CatsRaise.validatedNel {
    *   raise("error")
    * } should be(Validated.invalid(NonEmptyList.one("error")))
    * }}}
    *
    * @param block
    *   A computation that can raise errors of type `Error`
    * @tparam A
    *   The type of the value returned by the computation
    * @tparam Error
    *   The type of the logical error that can be raised by the computation
    * @return
    *   An [[ValidatedNel]] representing the outcome of the computation
    */
  inline def validatedNel[Error, A](inline block: Raise[Error] ?=> A): ValidatedNel[Error, A] =
    validated(
      Raise.withError[NonEmptyList[Error], Error, A](error => NonEmptyList.one(error))(block)
    )
}
