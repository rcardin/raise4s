package in.rcard.raise4s

import scala.util.{Failure, Success, Try}

/** Runs a computation `block` using [[Raise]], and return its outcome as [[Either]].
  *   - [[Right]] represents success,
  *   - [[Left]] represents logical failure.
  *
  * This function re-throws any exceptions thrown within the [[Raise]] block.
  *
  * @param block
  *   A computation that can raise errors of type `Error`
  * @tparam A
  *   The type of the value returned by the computation
  * @tparam Error
  *   The type of the logical error that can be raised by the computation
  * @return
  *   An [[Either]] representing the outcome of the computation
  */
def either[A, Error](block: Raise[Error] ?=> A): Either[Error, A] =
  fold(block, error => Left(error), value => Right(value))

object RaiseEitherPredef:
  extension [Error, A](either: Either[Error, A])(using r: Raise[Error])
    def bind(): A = either match
      case Right(a) => a
      case Left(e)  => r.raise(e)

class OptionRaise(val raise: Raise[Option[Nothing]]) extends Raise[Option[Nothing]]:
  override def raise(error: Option[Nothing]): Nothing = raise.raise(error)

object RaiseOptionPredef:
  extension [A](option: Option[A])(using optionRaise: Raise[None.type])
    def bind(): A = option.getOrElse(raise(None))

def option[A](block: OptionRaise ?=> A): Option[A] =
  fold(
    {
      given optionRaise: OptionRaise = new OptionRaise(new DefaultRaise()) // ???
      block(using optionRaise)
    },
    _ => None,
    Some(_)
  )

class TryRaise(val raise: Raise[Throwable]) extends Raise[Throwable]:
  override def raise(error: Throwable): Nothing = raise.raise(error)

object RaiseTryPredef:
  extension [A](tryValue: Try[A])(using tryRaise: Raise[Throwable])
    def bind(): A = tryValue match
      case Success(a) => a
      case Failure(e) => tryRaise.raise(e)

def $try[A](block: TryRaise ?=> A): Try[A] =
  fold(
    {
      given tryRaise: TryRaise = new TryRaise(new DefaultRaise())
      block(using tryRaise)
    },
    Failure(_),
    Success(_)
  )
