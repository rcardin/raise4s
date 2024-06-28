package in.rcard.raise4s.cats

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import in.rcard.raise4s.Raise

object CatsBind {
  extension [Error, A](validated: Validated[Error, A])
    inline def value(using Raise[Error]): A = validated match {
      case Valid(value)   => value
      case Invalid(error) => Raise.raise(error)
    }
}
