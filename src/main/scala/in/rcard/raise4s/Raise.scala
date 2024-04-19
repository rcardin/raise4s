package in.rcard.raise4s

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

/** Defines the main scope of the functions available on the `Raise` context.
  */
object Raise {

  /** Raises a _logical failure_ of type `Error`. This function behaves like a <em>return
    * statement</em>, immediately short-circuiting and terminating the computation.
    *
    * __Alternatives:__ Common ways to raise errors include: [[ensure]], [[ensureNotNull]], and
    * [[RaiseEitherPredef.bind]]. Consider using them to make your code more concise and expressive.
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
  def raise[Error](e: Error)(using raise: Raise[Error]): Nothing = raise.raise(e)

  /** Ensures that the `condition` is met; otherwise, [[Raise.raise]]s a logical failure of type
    * `Error`.
    *
    * In summary, this is a type-safe alternative to [[assert]], using the [[Raise]] API.
    *
    * <h2>Example</h2>
    * {{{
    * val actual: Int = fold(
    *   { ensure(42 < 0, () => "error") },
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
  def ensure[Error](condition: Boolean, raise: () => Error)(using r: Raise[Error]): Unit =
    if !condition then r.raise(raise())

  /** Ensures that the `value` is not null; otherwise, [[Raise.raise]]s a logical failure of type
    * `Error`.
    *
    * In summary, this is a type-safe alternative to [[require]], using the [[Raise]] API.
    *
    * <h2>Example</h2>
    * {{{
    * val actual: Int = fold(
    *   { ensureNotNull(null, () => "error") },
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
  def ensureNotNull[B, Error](value: B, raise: () => Error)(using r: Raise[Error]): B =
    if value == null then r.raise(raise())
    else value

  /** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type
    * `Error`, and recover by providing a transform `Error` into a fallback value of type `A`. <p>
    *
    * <h2>Example</h2>
    * {{{
    * val actual = recover(
    *   { raise("error") },
    *   error => 43
    * )
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
  def recover[Error, A](block: Raise[Error] ?=> A, recover: Error => A): A =
    fold(block, ex => throw ex, recover, identity)

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
  def recover[Error, A](
      block: Raise[Error] ?=> A,
      recover: Error => A,
      catchBlock: Throwable => A
  ): A =
    fold(block, catchBlock, recover, identity)

  /** Allows safely catching [[NonFatal]] exceptions without capturing exceptions like
    * [[OutOfMemoryError]] or [[VirtualMachineError]], etc.
    *
    * <h2>Example</h2>
    * {{{
    * val actual = $catch(
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
  def $catch[A](block: () => A, catchBlock: Throwable => A): A =
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
    *   withError[Int, String, Int](s => s.length, { raise("error") })
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
    * @tparam Error
    *   The type of the transformed logical error
    * @tparam OtherError
    *   The type of the logical error that can be raised and transformed
    * @tparam A
    *   The type of the result of the `block`
    * @return
    *   The result of the `block`
    */
  def withError[Error, OtherError, A](
      transform: OtherError => Error,
      block: Raise[OtherError] ?=> A
  )(using r: Raise[Error]): A =
    recover(block, otherError => r.raise(transform(otherError)))

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
  ): B = in.rcard.raise4s.fold(block, catchBlock, recover, transform)

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
  def fold[A, B, Error](
      block: Raise[Error] ?=> A,
      recover: (error: Error) => B,
      transform: (value: A) => B
  ): B = in.rcard.raise4s.fold(block, recover, transform)

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
    *   val x = one.bind()
    *   val y = recover(
    *     {
    *       left.bind()
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
  def either[A, Error](block: Raise[Error] ?=> A): Either[Error, A] = in.rcard.raise4s.either(block)

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
    *   val x = some.bind()
    *   val y = recover({ none.bind() }, { _ => 1 })
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
  def option[A](block: Raise[None.type] ?=> A): Option[A] = in.rcard.raise4s.option(block)

  /** Runs a computation `block` using [[Raise]], and return its outcome as [[Try]].
    *
    * <h2>Example</h2>
    * {{{
    * val one: Try[Int]     = Success(1)
    * val failure: Try[Int] = Failure(new Exception("error"))
    * val actual = $try {
    *   val x = one.bind()
    *   val y = recover({ failure.bind() }, { _ => 1 })
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
  def $try[A](block: Raise[Throwable] ?=> A): Try[A] = in.rcard.raise4s.$try(block)

  /** Accumulate the errors obtained by executing the `transform` over every element of `iterable`.
   * 
   * <h2>Example</h2>
   * {{{
   * val block: List[Int] raises List[String] = Raise.mapOrAccumulate(
   *   List(1, 2, 3, 4, 5),
   *   _ + 1
   * )
   * val actual = Raise.fold(
   *   block,
   *   error => fail(s"An error occurred: $error"),
   *   identity
   * )
   * actual shouldBe List(2, 3, 4, 5, 6)
   * }}}
    * 
    * @param iterable The collection of elements to transform
    * @param transform The transformation to apply to each element that can raise an error of type `Error`
    * @param r The Raise context
    * @tparam Error The type of the logical error that can be raised
    * @tparam A The type of the elements in the `iterable`
    * @tparam B The type of the transformed elements
    * @return A list of transformed elements
    */
  def mapOrAccumulate[Error, A, B](
      iterable: Iterable[A],
      transform: Raise[Error] ?=> A => B
  )(using r: Raise[List[Error]]): List[B] = in.rcard.raise4s.mapOrAccumulate(iterable, transform)
}
