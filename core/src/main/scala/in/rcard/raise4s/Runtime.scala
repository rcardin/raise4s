package in.rcard.raise4s

object Runtime {

  private[raise4s] inline def _run[Error, A](inline block: Raise[Error] ?=> A): Error | A =
    Raise.fold(block, identity, identity)
}
