package in.rcard.raise4s

private[raise4s] inline def _mapOrAccumulate[Error, A, B](iterable: Iterable[A])(
    inline transform: Raise[Error] ?=> A => B
)(using r: RaiseAcc[Error]): List[B] =
  val errors  = collection.mutable.ArrayBuffer.empty[Error]
  val results = collection.mutable.ArrayBuffer.empty[B]
  iterable.foreach(a =>
    Raise.fold(
      transform(a),
      error => errors += error,
      result => results += result
    )
  )
  if errors.isEmpty then results.toList
  else r.raise(errors.toList)

private[raise4s] inline def _mapOrAccumulate[Error, A, B](
    iterable: Iterable[A],
    inline combine: (Error, Error) => Error
)(
    inline transform: Raise[Error] ?=> A => B
)(using r: Raise[Error]): List[B] =
  val errors  = collection.mutable.ArrayBuffer.empty[Error]
  val results = collection.mutable.ArrayBuffer.empty[B]
  iterable.foreach(a =>
    Raise.fold(
      transform(a),
      error => errors += error,
      result => results += result
    )
  )
  if errors.isEmpty then results.toList
  else r.raise(errors.reduce(combine))

object RaiseIterableDef:
  extension [Error, A, B](iterable: Iterable[A])
    /** Maps the elements of the iterable to a new value of type `B` using the provided `transform`
      * function.
      *
      * @see
      *   [[Raise.mapOrAccumulate]]
      * @param transform
      *   The function to transform the elements of the iterable
      * @param r
      *   The Raise context to accumulate the errors
      * @return
      *   A list of the transformed elements of the iterable
      */
    inline def mapOrAccumulate(inline transform: Raise[Error] ?=> A => B)(using r: RaiseAcc[Error]): List[B] =
      Raise.mapOrAccumulate(iterable)(transform)

private[raise4s] inline def _zipOrAccumulate[Error, A, B, C, D, E, F, G, H, I, J](
    inline action1: Raise[Error] ?=> A,
    inline action2: Raise[Error] ?=> B,
    inline action3: Raise[Error] ?=> C,
    inline action4: Raise[Error] ?=> D,
    inline action5: Raise[Error] ?=> E,
    inline action6: Raise[Error] ?=> F,
    inline action7: Raise[Error] ?=> G,
    inline action8: Raise[Error] ?=> H,
    inline action9: Raise[Error] ?=> I
)(inline block: (A, B, C, D, E, F, G, H, I) => J)(using r: RaiseAcc[Error]): J = {
  val errors = collection.mutable.ArrayBuffer.empty[Error]
  val a: A = Raise.recover(action1) { newError =>
    errors += newError; null.asInstanceOf[A]
  }
  val b: B = Raise.recover(action2) { newError =>
    errors += newError; null.asInstanceOf[B]
  }
  val c: C = Raise.recover(action3) { newError =>
    errors += newError; null.asInstanceOf[C]
  }
  val d: D = Raise.recover(action4) { newError =>
    errors += newError; null.asInstanceOf[D]
  }
  val e: E = Raise.recover(action5) { newError =>
    errors += newError; null.asInstanceOf[E]
  }
  val f: F = Raise.recover(action6) { newError =>
    errors += newError; null.asInstanceOf[F]
  }
  val g: G = Raise.recover(action7) { newError =>
    errors += newError; null.asInstanceOf[G]
  }
  val h: H = Raise.recover(action8) { newError =>
    errors += newError; null.asInstanceOf[H]
  }
  val i: I = Raise.recover(action9) { newError =>
    errors += newError; null.asInstanceOf[I]
  }
  if errors.isEmpty then block(a, b, c, d, e, f, g, h, i)
  else r.raise(errors.toList)
}

private[raise4s] inline def _zipOrAccumulate[Error, A, B, C, D, E, F, G, H, I, J](
    inline combine: (Error, Error) => Error
)(
    inline action1: Raise[Error] ?=> A,
    inline action2: Raise[Error] ?=> B,
    inline action3: Raise[Error] ?=> C,
    inline action4: Raise[Error] ?=> D,
    inline action5: Raise[Error] ?=> E,
    inline action6: Raise[Error] ?=> F,
    inline action7: Raise[Error] ?=> G,
    inline action8: Raise[Error] ?=> H,
    inline action9: Raise[Error] ?=> I
)(inline block: (A, B, C, D, E, F, G, H, I) => J)(using r: Raise[Error]): J = {
  val errors = collection.mutable.ArrayBuffer.empty[Error]
  val a: A = Raise.recover(action1) { newError =>
    errors += newError; null.asInstanceOf[A]
  }
  val b: B = Raise.recover(action2) { newError =>
    errors += newError; null.asInstanceOf[B]
  }
  val c: C = Raise.recover(action3) { newError =>
    errors += newError; null.asInstanceOf[C]
  }
  val d: D = Raise.recover(action4) { newError =>
    errors += newError; null.asInstanceOf[D]
  }
  val e: E = Raise.recover(action5) { newError =>
    errors += newError; null.asInstanceOf[E]
  }
  val f: F = Raise.recover(action6) { newError =>
    errors += newError; null.asInstanceOf[F]
  }
  val g: G = Raise.recover(action7) { newError =>
    errors += newError; null.asInstanceOf[G]
  }
  val h: H = Raise.recover(action8) { newError =>
    errors += newError; null.asInstanceOf[H]
  }
  val i: I = Raise.recover(action9) { newError =>
    errors += newError; null.asInstanceOf[I]
  }
  if errors.isEmpty then block(a, b, c, d, e, f, g, h, i)
  else r.raise(errors.reduce(combine))
}
