package in.rcard.raise4s

object Runtime {
  extension [Error, A](r: => Raise[Error] ?=> A)
    def toEither: Either[Error, A] = Raise.fold(r, Left(_), Right(_))

    def toOption: Option[A] = Raise.fold(r, _ => None, Some(_))

    def run: Error | A = Raise.fold(r, identity, identity)

  // extension [A](fun: A raises Throwable) def toTry: Try[A] = Raise.fold(fun, Failure(_), Success(_))
}
