package in.rcard.raise4s

trait Raise[-Error]:
  def raise(e: Error): Nothing

def raise[Error](e: Error)(using raise: Raise[Error]): Nothing = raise.raise(e)