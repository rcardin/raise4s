package in.rcard.raise4s

import scala.util.control.NonFatal

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

private[raise4s] def mapOrAccumulate[Error, A, B](
    iterable: Iterable[A],
    transform: Raise[Error] ?=> A => B
)(using r: Raise[List[Error]]): List[B] =
  val errors  = collection.mutable.ArrayBuffer.empty[Error]
  val results = collection.mutable.ArrayBuffer.empty[B]
  iterable.foreach(a =>
    Raise.fold(
      transform(a),
      error => errors += error,
      result => results += result
    )
  )
  if errors.isEmpty then results.toList
  else r.raise(errors.toList)

object RaiseIterableDef:
  extension [Error, A, B](iterable: Iterable[A])
    def mapOrAccumulate(transform: Raise[Error] ?=> A => B)(using r: Raise[List[Error]]): List[B] =
      Raise.mapOrAccumulate(iterable, transform)

