package in.rcard.raise4s

import scala.util.control.{ControlThrowable, NoStackTrace, NonFatal}

private class DefaultRaise extends Raise[Any]:
  def raise(e: Any): Nothing = throw Raised(e)

private case class Raised[Error](original: Error) extends ControlThrowable with NoStackTrace

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

//noinspection NoTailRecursionAnnotation
def fold[A, B, Error](
    block: Raise[Error] ?=> A,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B = fold(block, ex => throw ex, recover, transform)
