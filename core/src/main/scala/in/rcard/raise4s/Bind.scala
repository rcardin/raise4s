package in.rcard.raise4s

import scala.util.{Failure, Success, Try}

object Bind {

  extension [Error, A](either: Either[Error, A])
    inline def value(using Raise[Error]): A = either match {
      case Right(value) => value
      case Left(error)  => Raise.raise(error)
    }
    
  extension [Error, A](list: List[Either[Error, A]])
    inline def value(using Raise[Error]): List[A] = list.map(_.value)

  extension [A](option: Option[A])
    inline def value(using Raise[None.type]): A = option match {
      case Some(value) => value
      case None        => Raise.raise(None)
    }

  extension [A](tryValue: Try[A])
    inline def value(using Raise[Throwable]): A = tryValue match {
      case Success(value)     => value
      case Failure(throwable) => Raise.raise(throwable)
    }
}
