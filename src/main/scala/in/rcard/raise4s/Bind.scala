package in.rcard.raise4s

import scala.util.{Failure, Success, Try}

object Bind {

  extension [Error, A](either: Either[Error, A])
    def value(using Raise[Error]): A = either match {
      case Right(value) => value
      case Left(error)  => Raise.raise(error)
    }

  extension [A](option: Option[A])
    def value(using Raise[None.type]): A = option match {
      case Some(value) => value
      case None        => Raise.raise(None)
    }

  extension [A](tryValue: Try[A])
    def value(using Raise[Throwable]): A = tryValue match {
      case Success(value)     => value
      case Failure(throwable) => Raise.raise(throwable)
    }
}
