package in.rcard.raise4s

object Runtime {

  private[raise4s] def _run[Error, A](block: Raise[Error] ?=> A): Error | A =
    Raise.fold(block, identity, identity)
}
