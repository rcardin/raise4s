package in.rcard.raise4s

import scala.util.control.NonFatal

trait Raise[-Error]:
  def raise(e: Error): Nothing

infix type raises[R, Error] = Raise[Error] ?=> R

def raise[Error](e: Error)(using raise: Raise[Error]): Nothing = raise.raise(e)

def ensure[Error](condition: Boolean, raise: () => Error)(using r: Raise[Error]): Unit =
  if !condition then r.raise(raise())

def ensureNotNull[B, Error](value: B, raise: () => Error)(using r: Raise[Error]): B =
  if value == null then r.raise(raise())
  else value

/** Execute the [[Raise]] context function resulting in `A` or any _logical error_ of type [Error],
  * and recover by providing a transform `Error` into a fallback value of type `A`. <p>
  * {{{
  * TODO
  * }}}
  *
  * @param block
  *   The block to execute
  * @param recover
  *   The function to transform the error into a fallback value
  * @tparam Error
  *   The type of the error that can be raised and recovered
  * @tparam A
  *   The type of the result of the block
  * @return
  *   The result of the block or the fallback value
  */
def recover[Error, A](block: Raise[Error] ?=> () => A, recover: Error => A): A =
  fold(block, ex => throw ex, recover, identity)

def recover[Error, A](
    block: Raise[Error] ?=> () => A,
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
    block: Raise[OtherError] ?=> () => A
)(using r: Raise[Error]) =
  recover(block, otherError => r.raise(transform(otherError)))
