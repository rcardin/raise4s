package in.rcard.raise4s

import scala.util.control.NonFatal

private[raise4s] def _fold[Error, B, A](
    block: Raise[Error] ?=> A,
    catchBlock: (throwable: Throwable) => B,
    recover: (error: Error) => B,
    transform: (value: A) => B
) = {
  given raise: Raise[Error] = new DefaultRaise

  try transform(block)
  catch
    case Raised(error) => recover(error.asInstanceOf[Error])
    case NonFatal(e)   => catchBlock(e)
    case e: Throwable  => throw e
}

@deprecated("Use Raise.fold instead", "0.0.3")
def fold[A, B, Error](
    block: Raise[Error] ?=> A,
    catchBlock: (throwable: Throwable) => B,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B =
  _fold(block, catchBlock, recover, transform)

@deprecated("Use Raise.fold instead", "0.0.3")
def fold[A, B, Error](
    block: Raise[Error] ?=> A,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B = _fold(block, ex => throw ex, recover, transform)
