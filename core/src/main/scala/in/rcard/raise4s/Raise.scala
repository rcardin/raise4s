package in.rcard.raise4s

import in.rcard.raise4s
import in.rcard.raise4s.Strategies.{RecoverWith, TraceWith, Traced, TracedRaise}

import scala.annotation.targetName
import scala.util.Try
import scala.util.control.{ControlThrowable, NoStackTrace, NonFatal}

trait Raise[-Error]:
  def raise(e: Error): Nothing

private[raise4s] case class Raised[Error](original: Error)
    extends ControlThrowable
    with NoStackTrace

private[raise4s] class DefaultRaise extends Raise[Any]:
  def raise(e: Any): Nothing = throw Raised(e)

infix type raises[R, Error] = Raise[Error] ?=> R

type RaiseAcc[Error] = Raise[List[Error]]

/** Defines the main scope of the functions available on the `Raise` context.
  */
object Raise {

  extension [A](a: => A)
    /** Extensions method version of the [[Raise.catching]] function.
      */
    // noinspection NoTailRecursionAnnotation
    @targetName("catchingThis")
    inline def catching(inline catchBlock: Throwable => A): A = Raise.catching(() => a)(catchBlock)

  /** Raises a _logical failure_ of type `Error`. This function behaves like a <em>return
    * statement</em>, immediately short-circuiting and terminating the computation.
    *
    * __Alternatives:__ Common ways to raise errors include: [[ensure]], [[ensureNotNull]], and
    * [[Bind.value]]. Consider using them to make your code more concise and expressive.
    *
    * __Handling raised errors:__ Refer to [[recover]]. <h2>Example</h2>
    * {{{
    * val actual: String = fold(
    *   { raise(MyError) },
    *   throwable => "Error: " + throwable.getMessage,
    *   error => "Error: " + error,
    *   value => "Value: " + value
    * )
    * }}}
    *
    * @param e
    *   An error of type `Error` that will short-circuit the computation. Behaves similarly to
    *   _return_ or _throw_.
    * @param raise
    *   The Raise context
    * @tparam Error
    *   The type of the logical error
    */
  inline def raise[Error](e: Error)(using raise: Raise[Error]): Nothing = raise.raise(e)

  /** Ensures that the `condition` is met; otherwise, [[Raise.raise]]s a logical failure of type
    * `Error`.
    *
    * In summary, this is a type-safe alternative to [[assert]], using the [[Raise]] API.
    *
    * <h2>Example</h2>
    * {{{
    * val actual: Int = fold(
    *   { ensure(42 < 0) { "error" },
    *   error => 43,
    *   value => 42
    * )
    * actual should be(43)
    * }}}
    *
    * @param condition
    *   The condition that must be true.
    * @param raise
    *   A lambda that produces an error of type `Error` when the `condition` is false.
    * @param r
    *   The Raise context
    * @tparam Error
    *   The type of the logical error
    */
  inline def ensure[Error](condition: Boolean)(raise: => Error)(using r: Raise[Error]): Unit =
    if !condition then r.raise(raise)

  /** Ensures that the `value` is not null; otherwise, [[Raise.raise]]s a logical failure of type
    * `Error`.
    *
    * In summary, this is a type-safe alternative to [[require]], using the [[Raise]] API.
    *
    * <h2>Example</h2>
    * {{{
    * val actual: Int = fold(
    *   { ensureNotNull(null) { "error" } },
    *   error => 43,
    *   value => 42
    * )
    * actual should be(43)
    * }}}
    *
    * @param value
    *   The value that must be non-null.
    * @param raise
    *   A lambda that produces an error of type `Error` when the `value` is null.
    * @param r
    *   The Raise context
    * @tparam B
    *   The type of the value
    * @tparam Error
    *   The type of the logical error
    * @return
    *   The value if it is not null
    */
  inline def ensureNotNull[B, Error](value: B)(raise: => Error)(using r: Raise[Error]): B =
    if value == null then r.raise(raise)
    else value

  /** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type
    * `Error`, and recover by providing a transform `Error` into a fallback value of type `A`. <p>
    *
    * <h2>Example</h2>
    * {{{
    * val actual = recover({ raise("error") }) {
    *   error => 43
    * }
    * actual should be(43)
    * }}}
    *
    * @param block
    *   The block to execute
    * @param recover
    *   The function to transform the error into a fallback value
    * @tparam Error
    *   The type of the error that can be raised and recovered
    * @tparam A
    *   The type of the result of the `block`
    * @return
    *   The result of the `block` or the fallback value
    */
  inline def recover[Error, A](inline block: Raise[Error] ?=> A)(inline recover: Error => A): A =
    Raise.fold(block, ex => throw ex, recover, identity)

  /** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type
    * `Error`, and `recover` by providing a transform `Error` into a fallback value of type `A`, or
    * `catchBlock` any unexpected exceptions by providing a transform [[Throwable]] into a fallback
    * value of type `A`.
    *
    * <h2>Example</h2>
    * {{{
    * val actual = recover(
    *   { raise("error") },
    *   error => 43,
    *   ex => 44
    * )
    * actual should be(43)
    * }}}
    *
    * @param block
    *   The block to execute
    * @param recover
    *   The function to transform the error into a fallback value
    * @param catchBlock
    *   The function to transform the exception into a fallback value
    * @tparam Error
    *   The type of the error that can be raised and recovered
    * @tparam A
    *   The type of the result of the `block`
    * @return
    *   The result of the `block`, the fallback value from the `recover` function, or the fallback
    *   value from the `catchBlock` function
    */
  inline def recover[Error, A](
      inline block: Raise[Error] ?=> A,
      inline recover: Error => A,
      inline catchBlock: Throwable => A
  ): A =
    Raise.fold(block, catchBlock, recover, identity)

  /** Allows safely catching [[NonFatal]] exceptions without capturing exceptions like
    * [[OutOfMemoryError]] or [[VirtualMachineError]], etc.
    *
    * <h2>Example</h2>
    * {{{
    * val actual = catching(
    *   () => throw new RuntimeException("error"),
    *   ex => 43
    * )
    * actual should be(43)
    * }}}
    *
    * @param block
    *   The block to execute
    * @param catchBlock
    *   The function to transform the exception into a fallback value
    * @tparam A
    *   The type of the result of the `block`
    * @return
    *   The result of the `block` or the fallback value
    */
  inline def catching[A](inline block: () => A)(inline catchBlock: Throwable => A): A =
    try block()
    catch
      case NonFatal(e) => catchBlock(e)
      case ex          => throw ex

  /** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type
    * `OtherError`, and transform any raised `OtherError` into `Error`, which is raised to the outer
    * [[Raise]].
    *
    * <h2>Example</h2>
    * {{{
    * val actual = either {
    *   withError[Int, String, Int](s => s.length) { raise("error") }
    * }
    * actual should be(Left(5))
    * }}}
    *
    * @param transform
    *   The function to transform the `OtherError` into `Error`
    * @param block
    *   The block to execute
    * @param r
    *   The Raise context
    * @tparam ToError
    *   The type of the transformed logical error
    * @tparam FromError
    *   The type of the logical error that can be raised and transformed
    * @tparam A
    *   The type of the result of the `block`
    * @return
    *   The result of the `block`
    */
  inline def withError[ToError, FromError, A](inline transform: FromError => ToError)(
      inline block: Raise[FromError] ?=> A
  )(using r: Raise[ToError]): A =
    recover(block) { otherError => r.raise(transform(otherError)) }

  /** The most general way to execute a computation using [[Raise]]. Depending on the outcome of the
    * `block`, one of the three continuations is run:
    *   - <em>success</em> `transform` result of [A] to a value of [B].
    *   - <em>raised</em> `recover` from raised value of `Error` to a value of `B`.
    *   - <em>exception</em> `catch` from [[Throwable]] by transforming value into `B`.
    *
    * This method should never be wrapped in `try`/`catch` as it will not throw any unexpected
    * errors, it will only result in fatal exceptions such as [[OutOfMemoryError]].
    *
    * <h2>Example</h2>
    * {{{
    * val actual: String = fold(
    *   { 42 },
    *   throwable => "Error: " + throwable.getMessage,
    *   error => "Error: " + error,
    *   value => "Value: " + value
    * )
    * actual shouldBe "Value: 42"
    * }}}
    *
    * @param block
    *   The block of code to execute that can raise an a logical type error
    * @param catchBlock
    *   The block of code to execute when a [[NonFatal]] exception is thrown
    * @param recover
    *   The block of code to execute when a logical error is raised
    * @param transform
    *   The block of code to execute when the block of code is executed successfully
    * @tparam A
    *   The type of the result of the execution of `block` lambda
    * @tparam B
    *   The type of the result of the `fold` method
    * @tparam Error
    *   The type of the logical error that can be raised by the `block` lambda
    */
  def fold[A, B, Error](
      block: Raise[Error] ?=> A,
      catchBlock: (throwable: Throwable) => B,
      recover: (error: Error) => B,
      transform: (value: A) => B
  ): B = {
    given raise: Raise[Error] = new DefaultRaise

    try transform(block)
    catch
      case Raised(error) => recover(error.asInstanceOf[Error])
      case NonFatal(e)   => catchBlock(e)
      case e: Throwable  => throw e
  }

  /** The most general way to execute a computation using [[Raise]]. Depending on the outcome of the
    * `block`, one of the two continuations is run:
    *   - <em>success</em> `transform` result of `A` to a value of `B`.
    *   - <em>raised</em> `recover` from raised value of `Error` to a value of `B`.
    *
    * This function re-throws any exceptions thrown within the [[Raise]] block.
    *
    * <h2>Example</h2>
    * {{{
    * val actual: String = fold(
    *   { 42 },
    *   error => "Error: " + error,
    *   value => "Value: " + value
    * )
    * actual shouldBe "Value: 42"
    * }}}
    *
    * @param block
    *   The block of code to execute that can raise an a logical type error
    * @param recover
    *   The block of code to execute when a logical error is raised
    * @param transform
    *   The block of code to execute when the block of code is executed successfully
    * @tparam A
    *   The type of the result of the execution of `block` lambda
    * @tparam B
    *   The type of the result of the `fold` method
    * @tparam Error
    *   The type of the logical error that can be raised by the `block` lambda
    */
  // noinspection NoTailRecursionAnnotation
  inline def fold[A, B, Error](
      inline block: Raise[Error] ?=> A,
      inline recover: (error: Error) => B,
      inline transform: (value: A) => B
  ): B = Raise.fold(block, ex => throw ex, recover, transform)

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[Either]].
    *   - [[Right]] represents success,
    *   - [[Left]] represents logical failure.
    *
    * This function re-throws any exceptions thrown within the [[Raise]] block.
    *
    * <h2>Example</h2>
    * {{{
    * val one: Either[Nothing, Int] = Right(1)
    * val left: Either[String, Int] = Left("error")
    * val actual = either {
    *   val x = one.value
    *   val y = recover(
    *     {
    *       left.value
    *     },
    *     { _ => 1 }
    *   )
    *   x + y
    * }
    * }}}
    *
    * @param block
    *   A computation that can raise errors of type `Error`
    * @tparam A
    *   The type of the value returned by the computation
    * @tparam Error
    *   The type of the logical error that can be raised by the computation
    * @return
    *   An [[Either]] representing the outcome of the computation
    */
  inline def either[A, Error](inline block: Raise[Error] ?=> A): Either[Error, A] = _either(block)

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[Option]].
    *   - [[Some]] represents success,
    *   - [[None]] represents logical failure. This function re-throws any exceptions thrown within
    *     the [[Raise]] block.
    *
    * <h2>Example</h2>
    * {{{
    * val some: Option[Int] = Some(1)
    * val none: Option[Int] = None
    * val actual = option {
    *   val x = some.value
    *   val y = recover({ none.value }, { _ => 1 })
    *   x + y
    * }
    * actual should be(Some(2))
    * }}}
    *
    * @param block
    *   A computation that can raise errors of type `None.type`
    * @tparam A
    *   The type of the value returned by the computation
    * @return
    *   An [[Option]] representing the outcome of the computation
    */
  inline def option[A](inline block: Raise[None.type] ?=> A): Option[A] = _option(block)

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[Try]].
    *
    * <h2>Example</h2>
    * {{{
    * val one: Try[Int]     = Success(1)
    * val failure: Try[Int] = Failure(new Exception("error"))
    * val actual = asTry {
    *   val x = one.value
    *   val y = recover({ failure.value }, { _ => 1 })
    *   x + y
    * }
    * actual should be(Success(2))
    * }}}
    *
    * @param block
    *   A computation that can raise errors of type `Throwable`
    * @tparam A
    *   The type of the value returned by the computation
    * @return
    *   An [[Try]] representing the outcome of the computation
    */
  inline def asTry[A](inline block: Raise[Throwable] ?=> A): Try[A] = _asTry(block)

  /** Accumulate the errors obtained by executing the `transform` over every element of `iterable`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.mapOrAccumulate(List(1, 2, 3, 4, 5)) {
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
  )(using r: RaiseAcc[Error]): List[B] = _mapOrAccumulate(iterable)(transform)

  /** Transform every element of `iterable` using the given `transform`, or accumulate all the
    * occurred errors using `combine`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(val errors: List[String])
    *   def combineErrors(error1: Errors, error2: Errors): Errors =
    *     Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors =
    *   Raise.mapOrAccumulate(List(1, 2, 3, 4, 5), combineErrors) { value =>
    *       if (value % 2 == 0) {
    *         Raise.raise(Errors(List(value.toString)))
    *       } else {
    *         value
    *       }
    *     }
    *
    * val actual = Raise.fold(
    *   block,
    *   identity,
    *   identity
    * )
    *
    * actual shouldBe Errors(List("2", "4"))
    * }}}
    *
    * @param iterable
    *   The collection of elements to transform
    * @param combine
    *   The function to combine the errors
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
  inline def mapOrAccumulate[Error, A, B](
      iterable: Iterable[A],
      inline combine: (Error, Error) => Error
  )(
      inline transform: Raise[Error] ?=> A => B
  )(using r: Raise[Error]): List[B] = _mapOrAccumulate(iterable, combine)(transform)

  /** Accumulate the errors from running `action1`, and `action2`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
    *   { 1 },
    *   { 2 }
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
  )(inline block: (A, B) => C)(using r: RaiseAcc[Error]): C =
    _zipOrAccumulate(action1, action2, {}, {}, {}, {}, {}, {}, {}) {
      (a: A, b: B, _, _, _, _, _, _, _) =>
        block(a, b)
    }

  /** Accumulate the errors from running `action1`, `action2`, and `action3`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
    *   { 1 },
    *   { 2 },
    *   { 3 }
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
  )(
      inline block: (A, B, C) => D
  )(using r: RaiseAcc[Error]): D =
    _zipOrAccumulate(action1, action2, action3, {}, {}, {}, {}, {}, {}) {
      (a: A, b: B, c: C, _, _, _, _, _, _) =>
        block(a, b, c)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, and `action4`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
  )(
      inline block: (A, B, C, D) => E
  )(using r: RaiseAcc[Error]): E =
    _zipOrAccumulate(action1, action2, action3, action4, {}, {}, {}, {}, {}) {
      (a: A, b: B, c: C, d: D, _, _, _, _, _) =>
        block(a, b, c, d)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, and `action5`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
  )(
      inline block: (A, B, C, D, E) => F
  )(using r: RaiseAcc[Error]): F =
    _zipOrAccumulate(action1, action2, action3, action4, action5, {}, {}, {}, {}) {
      (a: A, b: B, c: C, d: D, e: E, _, _, _, _) => block(a, b, c, d, e)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`, and
    * `action6`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
  )(
      inline block: (A, B, C, D, E, F) => G
  )(using r: RaiseAcc[Error]): G =
    _zipOrAccumulate(action1, action2, action3, action4, action5, action6, {}, {}, {}) {
      (a: A, b: B, c: C, d: D, e: E, f: F, _, _, _) => block(a, b, c, d, e, f)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, and `action7`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
  )(
      inline block: (A, B, C, D, E, F, G) => H
  )(using r: RaiseAcc[Error]): H =
    _zipOrAccumulate(action1, action2, action3, action4, action5, action6, action7, {}, {}) {
      (a: A, b: B, c: C, d: D, e: E, f: F, g: G, _, _) => block(a, b, c, d, e, f, g)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, and `action8`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
  )(
      inline block: (A, B, C, D, E, F, G, H) => I
  )(using r: RaiseAcc[Error]): I =
    _zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      {}
    ) { (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, Unit) =>
      block(a, b, c, d, e, f, g, h)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, `action8`, and `action9`.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
  )(inline block: (A, B, C, D, E, F, G, H, I) => J)(using r: RaiseAcc[Error]): J =
    _zipOrAccumulate(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      action9
    )(
      block
    )

  // ZIP COMBINE

  /** Accumulate the errors from running `action1`, and `action2`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
    *   { 1 },
    *   { 2 }
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
    * @param combine
    *   The function to combine the errors
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
  inline def zipOrAccumulate[Error, A, B, C](inline combine: (Error, Error) => Error)(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B
  )(inline block: (A, B) => C)(using r: Raise[Error]): C =
    _zipOrAccumulate(combine)(action1, action2, {}, {}, {}, {}, {}, {}, {}) {
      (a: A, b: B, _, _, _, _, _, _, _) =>
        block(a, b)
    }

  /** Accumulate the errors from running `action1`, `action2`, and `action3`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
    *   { 1 },
    *   { 2 },
    *   { 3 }
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
    * @param combine
    *   The function to combine the errors
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
  inline def zipOrAccumulate[Error, A, B, C, D](inline combine: (Error, Error) => Error)(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C
  )(
      inline block: (A, B, C) => D
  )(using r: Raise[Error]): D =
    _zipOrAccumulate(combine)(action1, action2, action3, {}, {}, {}, {}, {}, {}) {
      (a: A, b: B, c: C, _, _, _, _, _, _) =>
        block(a, b, c)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, and `action4`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)
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
    * @param combine
    *   The function to combine the errors
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
  inline def zipOrAccumulate[Error, A, B, C, D, E](inline combine: (Error, Error) => Error)(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D
  )(
      inline block: (A, B, C, D) => E
  )(using r: Raise[Error]): E =
    _zipOrAccumulate(combine)(action1, action2, action3, action4, {}, {}, {}, {}, {}) {
      (a: A, b: B, c: C, d: D, _, _, _, _, _) =>
        block(a, b, c, d)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, and `action5`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
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
    * @param combine
    *   The function to combine the errors
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
  inline def zipOrAccumulate[Error, A, B, C, D, E, F](inline combine: (Error, Error) => Error)(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E
  )(
      inline block: (A, B, C, D, E) => F
  )(using r: Raise[Error]): F =
    _zipOrAccumulate(combine)(action1, action2, action3, action4, action5, {}, {}, {}, {}) {
      (a: A, b: B, c: C, d: D, e: E, _, _, _, _) => block(a, b, c, d, e)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`, and
    * `action6`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
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
    * @param combine
    *   The function to combine the errors
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
  inline def zipOrAccumulate[Error, A, B, C, D, E, F, G](inline combine: (Error, Error) => Error)(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F
  )(
      inline block: (A, B, C, D, E, F) => G
  )(using r: Raise[Error]): G =
    _zipOrAccumulate(combine)(action1, action2, action3, action4, action5, action6, {}, {}, {}) {
      (a: A, b: B, c: C, d: D, e: E, f: F, _, _, _) => block(a, b, c, d, e, f)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, and `action7`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
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
    * @param combine
    *   The function to combine the errors
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
      inline combine: (Error, Error) => Error
  )(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G
  )(
      inline block: (A, B, C, D, E, F, G) => H
  )(using r: Raise[Error]): H =
    _zipOrAccumulate(combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      {},
      {}
    ) { (a: A, b: B, c: C, d: D, e: E, f: F, g: G, _, _) =>
      block(a, b, c, d, e, f, g)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, and `action8`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
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
    * @param combine
    *   The function to combine the errors
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
      inline combine: (Error, Error) => Error
  )(
      inline action1: Raise[Error] ?=> A,
      inline action2: Raise[Error] ?=> B,
      inline action3: Raise[Error] ?=> C,
      inline action4: Raise[Error] ?=> D,
      inline action5: Raise[Error] ?=> E,
      inline action6: Raise[Error] ?=> F,
      inline action7: Raise[Error] ?=> G,
      inline action8: Raise[Error] ?=> H
  )(
      inline block: (A, B, C, D, E, F, G, H) => I
  )(using r: Raise[Error]): I =
    _zipOrAccumulate(combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      {}
    ) { (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, _) =>
      block(a, b, c, d, e, f, g, h)
    }

  /** Accumulate the errors from running `action1`, `action2`, `action3`, `action4`, `action5`,
    * `action6`, `action7`, `action8`, and `action9`.
    *
    * <h2>Example</h2>
    * {{{
    * case class Errors(errors: List[String])
    * def combineErrors(error1: Errors, error2: Errors): Errors =
    *   Errors(error1.errors ++ error2.errors)
    *
    * val block: List[Int] raises Errors = Raise.zipOrAccumulate(combineErrors)(
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
    * @param combine
    *   Function to combine the errors
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
      inline combine: (Error, Error) => Error
  )(
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
    _zipOrAccumulate(combine)(
      action1,
      action2,
      action3,
      action4,
      action5,
      action6,
      action7,
      action8,
      action9
    )(
      block
    )

  /** Execute a block of code that can raise a logical error and return the result or the error as a
    * union type `Error | A`.
    *
    * <h2>Example</h2>
    * {{{
    * val happyPathBlock: Int raises String = 42
    * val result: String | Int = Runtime._run(happyPathBlock)
    * result should be(42)
    * }}}
    *
    * @param block
    *   The block of code to execute that can raise an a logical type error
    * @tparam Error
    *   The type of the logical error that can be raised by the `block` lambda
    * @tparam A
    *   The type of the result of the execution of `block` lambda
    * @return
    *   The result of the execution of the `block` lambda or the logical error
    */
  inline def run[Error, A](inline block: Raise[Error] ?=> A): Error | A =
    raise4s.Runtime._run(block)

  /** Execute a block of code that can raise a logical error and return the result or tries to
    * recover from the error using the provided recover block.
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
    * @param block
    *   The block of code to execute that can raise an a logical type error
    * @param recoverBlock
    *   The block of code to execute to recover from the error
    * @tparam Error
    *   The type of the logical error that can be raised by the `block` lambda
    * @tparam A
    *   The type of the result of the execution of `block` lambda
    * @return
    *   The result of the execution of the `block` lambda or the result of the recover block
    *
    * @see
    *   [[RecoverWith]]
    */
  inline def recoverable[Error, A](inline block: Raise[Error] ?=> A)(using
      inline recoverBlock: RecoverWith[Error, A]
  ): A =
    Raise.recover(block)(error => recoverBlock.recover(error))

  /** Execute a `block` of code that can raise an error and return the result or the given `default`
    * value if an error is raised.
    *
    * <h2>Example</h2>
    * {{{
    * val actual = Raise.either {
    *   Raise.withDefault(43) { Raise.raise("error") }
    * }
    * actual should be(Right(43))
    * }}}
    *
    * @param default
    *   The default value to return if an error is raised
    * @param block
    *   The block of code to execute that can raise an error
    * @tparam Error
    *   The type of the logical error that can be raised by the `block` lambda
    * @tparam A
    *   The type of the result of the execution of `block` lambda
    * @return
    *   The result of the execution of the `block` lambda or the `default` value
    */
  inline def withDefault[Error, A](default: A)(inline block: Raise[Error] ?=> A): A =
    Raise.recover(block)(error => default)

  inline def traced[Error, A](
      inline block: Raise[Error] ?=> A
  )(using inline tracing: TraceWith[Error]): Raise[Error] ?=> A = {
    try {
      given tracedRaise: Raise[Error] = new TracedRaise
      block
    } catch
      case traced: Traced[Error] =>
        tracing.trace(traced)
        Raise.raise(traced.original)
  }
}
