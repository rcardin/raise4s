package in.rcard.raise4s

import scala.util.control.NonFatal

trait Raise[-Error]:
  def raise(e: Error): Nothing

infix type raises[R, Error] = Raise[Error] ?=> R

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
  *   An error of type `Error` that will short-circuit the computation. Behaves similarly to _return_
  *   or _throw_.
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

/** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type `Error`,
  * and recover by providing a transform `Error` into a fallback value of type `A`. <p>
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

/** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type `Error`,
  * and `recover` by providing a transform `Error` into a fallback value of type `A`,
  * or `catchBlock` any unexpected exceptions by providing a transform [[Throwable]] into a fallback value of type `A`.
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
  * @param block The block to execute
  * @param recover The function to transform the error into a fallback value
  * @param catchBlock The function to transform the exception into a fallback value
  * @tparam Error The type of the error that can be raised and recovered
  * @tparam A The type of the result of the `block`
  * @return The result of the `block`, the fallback value from the `recover` function, or the fallback value from the `catchBlock` function
  */
def recover[Error, A](
    block: Raise[Error] ?=> A,
    recover: Error => A,
    catchBlock: Throwable => A
): A =
  fold(block, catchBlock, recover, identity)

def $catch[A](block: () => A, catchBlock: Throwable => A): A =
  try block()
  catch
    case NonFatal(e) => catchBlock(e)
    case ex          => throw ex

def withError[Error, OtherError, A](
    transform: OtherError => Error,
    block: Raise[OtherError] ?=> A
)(using r: Raise[Error]): A =
  recover(block, otherError => r.raise(transform(otherError)))
