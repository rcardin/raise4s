package in.rcard.raise4s

trait Raise[-Error]:
  def raise(e: Error): Nothing
