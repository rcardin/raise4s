package in.rcard.raise4s

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
}
