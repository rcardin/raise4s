package in.rcard.raise4s

import scala.annotation.experimental
import scala.reflect.TypeTest
import scala.util.{Failure, Success, Try}

/** Runs a computation `block` using [[Raise]], and return its outcome as [[Either]].
  *   - [[Right]] represents success,
  *   - [[Left]] represents logical failure.
  *
  * This function re-throws any exceptions thrown within the [[Raise]] block.
  *
  * <h2>Example</h2>
  * {{{
  * val one: Either[Nothing, Int] = Right(1)
  * val left: Either[String, Int] = Left("error")
  * val actual = either {
  *   val x = one.bind()
  *   val y = recover(
  *     {
  *       left.bind()
  *     },
  *     { _ => 1 }
  *   )
  *   x + y
  * }
  * }}}
  *
  * @param block
  *   A computation that can raise errors of type `Error`
  * @tparam A
  *   The type of the value returned by the computation
  * @tparam Error
  *   The type of the logical error that can be raised by the computation
  * @return
  *   An [[Either]] representing the outcome of the computation
  */
def either[A, Error](block: Raise[Error] ?=> A): Either[Error, A] =
  fold(block, error => Left(error), value => Right(value))

object RaiseEitherPredef:
  extension [Error, A](either: Either[Error, A])(using r: Raise[Error])
    def bind(): A = either match
      case Right(a) => a
      case Left(e)  => r.raise(e)

object RaiseOptionPredef:
  extension [A](option: Option[A])(using optionRaise: Raise[None.type])
    def bind(): A = option.getOrElse(raise(None))

/** Runs a computation `block` using [[Raise]], and return its outcome as [[Option]].
  *   - [[Some]] represents success,
  *   - [[None]] represents logical failure.
  * This function re-throws any exceptions thrown within the [[Raise]] block.
  *
  * <h2>Example</h2>
  * {{{
  * val some: Option[Int] = Some(1)
  * val none: Option[Int] = None
  * val actual = option {
  *   val x = some.bind()
  *   val y = recover({ none.bind() }, { _ => 1 })
  *   x + y
  * }
  * actual should be(Some(2))
  * }}}
  *
  * @param block
  *   A computation that can raise errors of type `None.type`
  * @tparam A
  *   The type of the value returned by the computation
  * @return
  *   An [[Option]] representing the outcome of the computation
  */
def option[A](block: Raise[None.type] ?=> A): Option[A] =
  fold(
    {
      given optionRaise: Raise[None.type] = new DefaultRaise() // ???
      block(using optionRaise)
    },
    _ => None,
    Some(_)
  )

object RaiseTryPredef:
  extension [A](tryValue: Try[A])(using tryRaise: Raise[Throwable])
    def bind(): A = tryValue match
      case Success(a) => a
      case Failure(e) => tryRaise.raise(e)

/** Runs a computation `block` using [[Raise]], and return its outcome as [[Try]].
  *
  * <h2>Example</h2>
  * {{{
  * val one: Try[Int]     = Success(1)
  * val failure: Try[Int] = Failure(new Exception("error"))
  * val actual = $try {
  *   val x = one.bind()
  *   val y = recover({ failure.bind() }, { _ => 1 })
  *   x + y
  * }
  * actual should be(Success(2))
  * }}}
  *
  * @param block
  *   A computation that can raise errors of type `Throwable`
  * @tparam A
  *   The type of the value returned by the computation
  * @return
  *   An [[Try]] representing the outcome of the computation
  */
def $try[A](block: Raise[Throwable] ?=> A): Try[A] =
  fold(
    {
      given tryRaise: Raise[Throwable] = new DefaultRaise()
      block(using tryRaise)
    },
    Failure(_),
    Success(_)
  )

object RaiseUnionPredef:
  extension [Error, A](unionType: Error | A)
    @experimental
    def toEither(using aTest: TypeTest[Any, A], eTest: TypeTest[Any, Error]): Either[Error, A] = unionType match
      case error: Error => Left(error)
      case a: A => Right(a)
      
    @experimental
    def toOption(using aTest: TypeTest[Any, A], eTest: TypeTest[Any, Error]): Option[A] = unionType match
      case error: Error => None
      case a: A => Some(a)