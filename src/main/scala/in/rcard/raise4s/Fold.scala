package in.rcard.raise4s

import scala.util.control.{ControlThrowable, NoStackTrace}

private class DefaultRaise extends Raise[Any]:
  def raise(e: Any): Nothing = throw Raised(e)

private class Raised[Error](val original: Error) extends ControlThrowable with NoStackTrace

def fold[A, B, Error](
    block: Raise[Error] ?=> () => A,
    catchBlock: (throwable: Throwable) => B,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B =
  given raise: Raise[Error] = new DefaultRaise
  try transform(block())
  catch
    case e: Raised[Error] => recover(e.original)
    case e: Throwable     => catchBlock(e)
end fold
