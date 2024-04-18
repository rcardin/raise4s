package in.rcard.raise4s

import scala.annotation.experimental
import scala.util.control.{ControlThrowable, NoStackTrace, NonFatal}

private class DefaultRaise extends Raise[Any]:
  def raise(e: Any): Nothing = throw Raised(e)

private case class Raised[Error](original: Error) extends ControlThrowable with NoStackTrace

/** The most general way to execute a computation using [[Raise]]. Depending on the outcome of the
  * `block`, one of the three continuations is run:
  *   - <em>success</em> `transform` result of [A] to a value of [B].
  *   - <em>raised</em> `recover` from raised value of `Error` to a value of `B`.
  *   - <em>exception</em> `catch` from [[Throwable]] by transforming value into
  * This method should never be wrapped in `try`/`catch` as it will not throw any unexpected errors,
  * it will only result in fatal exceptions such as [[OutOfMemoryError]].
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
): B =
  given raise: Raise[Error] = new DefaultRaise
  try transform(block)
  catch
    case Raised(error) => recover(error.asInstanceOf[Error])
    case NonFatal(e)   => catchBlock(e)
    case e: Throwable  => throw e
end fold

/** The most general way to execute a computation using [[Raise]]. Depending on the outcome of the
  * `block`, one of the two continuations is run: <ul> <li><em>success</em> `transform` result of
  * `A` to a value of `B`.</li> <li><em>raised</em> `recover` from raised value of `Error` to a
  * value of `B`.</li> </ul> This function re-throws any exceptions thrown within the [[Raise]]
  * block.
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
//noinspection NoTailRecursionAnnotation
def fold[A, B, Error](
    block: Raise[Error] ?=> A,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B = fold(block, ex => throw ex, recover, transform)

@experimental
def runRaise[Error, A](block: Raise[Error] ?=> A): Error | A =
  given raise: Raise[Error] = new DefaultRaise
  try block(using raise)
  catch
    case Raised(error)        => error.asInstanceOf[Error]
    case throwable: Throwable => throw throwable

@experimental
class LeakedRaisedErrorException[Error](val error: Error)
    extends IllegalStateException(s"The error $error was risen but not expected")
    with NoStackTrace

@experimental
def runRaiseUnsafe[Error, A](block: Raise[Error] ?=> A): A =
  given raise: Raise[Error] = new DefaultRaise
  try block(using raise)
  catch
    case Raised(error)        => throw LeakedRaisedErrorException(error)
    case throwable: Throwable => throw throwable
