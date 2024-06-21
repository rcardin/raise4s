package in.rcard.raise4s

import scala.util.{Failure, Success, Try}

private[raise4s] inline def _either[Error, A](inline block: Raise[Error] ?=> A): Either[Error, A] = {
  Raise.fold(block, error => Left(error), value => Right(value))
}

private[raise4s] inline def _option[A](inline block: Raise[None.type] ?=> A): Option[A] = {
  Raise.fold(
    block,
    _ => None,
    Some(_)
  )
}

private[raise4s] inline def _asTry[A](inline block: Raise[Throwable] ?=> A): Try[A] = {
  Raise.fold(
    block,
    Failure(_),
    Success(_)
  )
}

object RaiseAnyPredef:
  extension [A](value: A) inline def succeed: Raise[Nothing] ?=> A = { value }

  extension [Error](value: Error)(using raise: Raise[Error])
    inline def raise[A]: Raise[Error] ?=> A = raise.raise(value)
