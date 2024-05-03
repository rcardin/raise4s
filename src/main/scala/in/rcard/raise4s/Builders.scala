package in.rcard.raise4s

import in.rcard.raise4s.Bind.value

import scala.util.{Failure, Success, Try}

private[raise4s] inline def _either[Error, A](inline block: Raise[Error] ?=> A): Either[Error, A] = {
  Raise.fold(block, error => Left(error), value => Right(value))
}

object RaiseEitherPredef:
  extension [Error, A](either: Either[Error, A])(using r: Raise[Error])
    @deprecated("Use the extension method 'value' defined in Bind scope instead", "0.0.5")
    def bind(): A = either.value

object RaiseOptionPredef:
  extension [A](option: Option[A])(using optionRaise: Raise[None.type])
    @deprecated("Use the extension method 'value' defined in Bind scope instead", "0.0.5")
    def bind(): A = option.value

private[raise4s] inline def _option[A](inline block: Raise[None.type] ?=> A): Option[A] = {
  Raise.fold(
    block,
    _ => None,
    Some(_)
  )
}

object RaiseTryPredef:
  extension [A](tryValue: Try[A])(using tryRaise: Raise[Throwable])
    @deprecated("Use the extension method 'value' defined in Bind scope instead", "0.0.5")
    def bind(): A = tryValue.value

private[raise4s] inline def _asTry[A](inline block: Raise[Throwable] ?=> A): Try[A] = {
  Raise.fold(
    block,
    Failure(_),
    Success(_)
  )
}

object RaiseAnyPredef:
  extension [A](value: A) def succeed: Raise[Nothing] ?=> A = { value }

  extension [Error](value: Error)(using raise: Raise[Error])
    def raise[A]: Raise[Error] ?=> A = raise.raise(value)
