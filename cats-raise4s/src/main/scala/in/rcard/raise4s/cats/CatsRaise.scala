package in.rcard.raise4s.cats

import cats.Semigroup
import in.rcard.raise4s.Raise

object CatsRaise {
  def mapOrAccumulate[Error: Semigroup, A, B](iterable: Iterable[A])(
      transform: Raise[Error] ?=> A => B
  )(using r: Raise[Error]): List[B] =
    Raise.mapOrAccumulate(iterable, Semigroup[Error].combine)(transform)
}
