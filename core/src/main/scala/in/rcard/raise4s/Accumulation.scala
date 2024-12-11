package in.rcard.raise4s

import in.rcard.raise4s.Raise.raise

import scala.collection.mutable.ArrayBuffer

object Accumulation {

  /** The scope needed to accumulate errors using the [[accumulate]] function
    * @tparam Error
    *   The type of the errors to accumulate
    */
  class AccumulateScope[Error] {
    private[raise4s] val _errors     = ArrayBuffer.empty[Error]
    def errors: List[Error]          = _errors.toList
    def addError(error: Error): Unit = _errors += error
    def hasErrors: Boolean           = _errors.nonEmpty
  }

  /** Conversion from a [[Value]] to its contained value. If the [[Value]] contains errors, it will
    * raise them. There is no need to import this conversion as it is provided by default inside the
    * [[AccumulateScope]] scope.
    */
  given valueConversion[Error: RaiseAcc, A]: Conversion[Value[Error, A], A] with
    def apply(toConvert: Value[Error, A]): A = toConvert.value

  /** Accumulates the errors of the executions in the `block` lambda and raises all of them if any
    * error is found. In detail, the `block` lambda must be a series of statements using the
    * [[accumulating]] function to accumulate possible raised errors.
    *
    * <h2>Example</h2>
    * {{{
    * def validateName(name: String): String raises String = {
    *   ensure(name.nonEmpty)("Name cannot be empty")
    *   name
    * }
    * def validateAge(age: Int): Int raises String = {
    *   ensure(age >= 0)("Age cannot be negative")
    *   age
    * }
    *
    * val person: Person raises List[String] = accumulate {
    *   val name = accumulating { validateName("") }
    *   val age  = accumulating { validateAge(-1) }
    *   Person(name, age)
    * }
    * }}}
    *
    * Errors are accumulated in the order they are raised the first time one of the accumulated
    * values is accessed.
    *
    * @param block
    *   The block of code that can raise multiple errors
    * @tparam Error
    *   The type of the errors to accumulate
    * @tparam A
    *   The type of the value to return if no errors are raised
    * @return
    *   The value of the block if no errors are raised
    */
  inline def accumulate[Error, A](block: AccumulateScope[Error] ?=> A): RaiseAcc[Error] ?=> A = {

    import scala.language.implicitConversions
    given acc: AccumulateScope[Error] = AccumulateScope()
    val result: A                     = block(using acc)
    result
  }

  /** Represents a value that can be either a value or a list of errors raised inside an
    * [[accumulate]] block by the [[accumulating]] function.
    *
    * @see
    *   [[valueConversion]]
    */
  class Value[Error, A](
      private val _value: A,
      private val accumulateScope: AccumulateScope[Error]
  ) {
    inline def value: RaiseAcc[Error] ?=> A =
      if (accumulateScope.hasErrors) raise(accumulateScope.errors)
      else _value
  }

  object Value {
    inline def apply[Error, A](value: A)(using scope: AccumulateScope[Error]): Value[Error, A] =
      new Value(value: A, scope)

    inline def apply[Error, A](using scope: AccumulateScope[Error]): Value[Error, A] =
      new Value(null.asInstanceOf[A], scope)
  }

  /** Accumulates the errors of the executions in the `block` lambda and returns a [[Value]] that
    * can be either a value or a list of errors. The function is intended to be used inside an
    * [[accumulate]] block.
    *
    * @see
    *   [[accumulate]]
    */
  inline def accumulating[Error, A](
      inline block: Raise[Error] ?=> A
  )(using scope: AccumulateScope[Error]): Value[Error, A] = {
    Raise.recover({
      val a = block
      Value(value = a)
    }) { error =>
      scope.addError(error)
      Value(using scope)
    }
  }

  /** A type class to convert a container `MV[_]` of [[Value]]s to a container of values. The error
    * raised during the conversion must be accumulated into a container `ME[_]` of errors.
    * @tparam ME
    *   The type of the container of errors
    * @tparam MV
    *   The type of the container of [[Value]]s
    */
  trait ValuesConverter[ME[_], MV[_]] {
    private type RaiseM[Error] = Raise[ME[Error]]
    def convert[Error: RaiseM, A](convertible: MV[Value[Error, A]]): MV[A]
  }

  /** A type class to convert a container `M[_]` of [[Value]]s to a container of values accumulating
    * errors into a [[RaiseAcc]] container.
    * @tparam M
    *   The type of the container of [[Value]]s
    */
  trait RaiseAccValuesConverter[M[_]] extends ValuesConverter[List, M] {
    def convert[Error: RaiseAcc, A](convertible: M[Value[Error, A]]): M[A]
  }

  /** A type class instance to convert a container `List[Value[Error, A]]` to a container `List[A]`
    * accumulating errors into a [[RaiseAcc]] container.
    *
    * @see
    *   [[Value]]
    */
  given listRaiseAccValuesConverter: RaiseAccValuesConverter[List] with
    def convert[Error: RaiseAcc, A](convertible: List[Value[Error, A]]): List[A] =
      convertible.map(_.value)

  /** Implicit conversion between a container `M[Value[Error, A]]` and a container `M[A]`
    * accumulating errors into a [[RaiseAcc]] container.
    *
    * <h2>Example</h2>
    * {{{
    * val block: List[Int] raises List[String] = accumulate {
    *   List(1, 2, 3, 4, 5).map[Accumulation.Value[String, Int]] { i =>
    *     accumulating {
    *       if (i % 2 == 0) {
    *         Raise.raise(i.toString)
    *       } else {
    *         i
    *       }
    *     }
    *   }
    * }
    *
    * val actual = Raise.fold(
    *   block,
    *   identity,
    *   identity
    * )
    *
    * actual shouldBe List("2", "4")
    * }}}
    *
    * @see
    *   [[Value]]
    */
  given raiseAccValuesConversion[Error: RaiseAcc, A, M[_]: RaiseAccValuesConverter]
      : Conversion[M[Value[Error, A]], M[A]] with
    def apply(convertible: M[Value[Error, A]]): M[A] =
      summon[RaiseAccValuesConverter[M]].convert(convertible)
}
