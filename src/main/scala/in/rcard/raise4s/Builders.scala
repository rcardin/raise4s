package in.rcard.raise4s

import scala.util.{Failure, Success, Try}

private[raise4s] def _either[Error, A](block: Raise[Error] ?=> A): Either[Error, A] = {
  Raise.fold(block, error => Left(error), value => Right(value))
}

@deprecated("Use the Raise.either method instead", "0.0.3")
def either[A, Error](block: Raise[Error] ?=> A): Either[Error, A] =
  _either(block)

object RaiseEitherPredef:
  extension [Error, A](either: Either[Error, A])(using r: Raise[Error])
    def bind(): A = either match
      case Right(a) => a
      case Left(e)  => r.raise(e)

object RaiseOptionPredef:
  extension [A](option: Option[A])(using optionRaise: Raise[None.type])
    def bind(): A = option.getOrElse(Raise.raise(None))

private[raise4s] def _option[A](block: Raise[None.type] ?=> A): Option[A] = {
  Raise.fold(
    block,
    _ => None,
    Some(_)
  )
}

@deprecated("Use the Raise.option method instead", "0.0.3")
def option[A](block: Raise[None.type] ?=> A): Option[A] =
  _option(block)

object RaiseTryPredef:
  extension [A](tryValue: Try[A])(using tryRaise: Raise[Throwable])
    def bind(): A = tryValue match
      case Success(a) => a
      case Failure(e) => tryRaise.raise(e)

private[raise4s] def _asTry[A](block: Raise[Throwable] ?=> A): Try[A] = {
  Raise.fold(
    block,
    Failure(_),
    Success(_)
  )
}

@deprecated("Use the Raise.asTry method instead", "0.0.3")
def asTry[A](block: Raise[Throwable] ?=> A): Try[A] =
  _asTry(block)

object RaiseAnyPredef:
  extension [A](value: A) def succeed: Raise[Nothing] ?=> A = { value }

  extension [Error](value: Error)(using raise: Raise[Error])
    def raise[A]: Raise[Error] ?=> A = raise.raise(value)
