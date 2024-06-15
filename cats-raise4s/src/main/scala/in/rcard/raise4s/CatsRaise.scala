package in.rcard.raise4s

import cats.Semigroup

object CatsRaise {
  def mapOrAccumulate[Error: Semigroup, A, B](iterable: Iterable[A])(
      transform: Raise[Error] ?=> A => B
  )(using r: Raise[Error]): List[B] =
    _mapOrAccumulate(iterable, Semigroup[Error].combine)(transform)
}
