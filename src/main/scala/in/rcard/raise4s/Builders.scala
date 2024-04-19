package in.rcard.raise4s

import scala.util.{Failure, Success, Try}

def either[A, Error](block: Raise[Error] ?=> A): Either[Error, A] =
  fold(block, error => Left(error), value => Right(value))

object RaiseEitherPredef:
  extension [Error, A](either: Either[Error, A])(using r: Raise[Error])
    def bind(): A = either match
      case Right(a) => a
      case Left(e)  => r.raise(e)

object RaiseOptionPredef:
  extension [A](option: Option[A])(using optionRaise: Raise[None.type])
    def bind(): A = option.getOrElse(Raise.raise(None))

def option[A](block: Raise[None.type] ?=> A): Option[A] =
  fold(
    {
      given optionRaise: Raise[None.type] = new DefaultRaise() // ???
      block(using optionRaise)
    },
    _ => None,
    Some(_)
  )

object RaiseTryPredef:
  extension [A](tryValue: Try[A])(using tryRaise: Raise[Throwable])
    def bind(): A = tryValue match
      case Success(a) => a
      case Failure(e) => tryRaise.raise(e)

def $try[A](block: Raise[Throwable] ?=> A): Try[A] =
  fold(
    {
      given tryRaise: Raise[Throwable] = new DefaultRaise()
      block(using tryRaise)
    },
    Failure(_),
    Success(_)
  )

object RaiseAnyPredef:
  extension [A](value: A) def succeed: Raise[Nothing] ?=> A = { value }

  extension [Error](value: Error)(using raise: Raise[Error])
    def raise[A]: Raise[Error] ?=> A = raise.raise(value)
