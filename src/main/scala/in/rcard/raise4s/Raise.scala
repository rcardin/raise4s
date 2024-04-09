package in.rcard.raise4s

trait Raise[-Error]:
  def raise(e: Error): Nothing

infix type raises[R, Error] = Raise[Error] ?=> R

def raise[Error](e: Error)(using raise: Raise[Error]): Nothing = raise.raise(e)

def ensure[Error](condition: Boolean, raise: () => Error)(using r: Raise[Error]): Unit =
  if !condition then r.raise(raise())

def ensureNotNull[B, Error](value: B, raise: () => Error)(using r: Raise[Error]): B =
  if value == null then r.raise(raise())
  else value
