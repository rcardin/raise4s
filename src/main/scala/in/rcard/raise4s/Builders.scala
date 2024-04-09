package in.rcard.raise4s

def either[A, Error](block: Raise[Error] ?=> () => A): Either[Error, A] =
  fold(block, error => Left(error), value => Right(value))
