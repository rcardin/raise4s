package in.rcard.raise4s

import scala.util.control.{ControlThrowable, NoStackTrace, NonFatal}

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

//noinspection NoTailRecursionAnnotation
def fold[A, B, Error](
    block: Raise[Error] ?=> A,
    recover: (error: Error) => B,
    transform: (value: A) => B
): B = fold(block, ex => throw ex, recover, transform)

//def runRaise[Error, A](block: Raise[Error] ?=> A): Error | A =
//  given raise: Raise[Error] = new DefaultRaise
//  try block(using raise)
//  catch
//    case Raised(error)        => error.asInstanceOf[Error]
//    case throwable: Throwable => throw throwable