package in.rcard.raise4s

import in.rcard.raise4s.Bind.value
import in.rcard.raise4s.Raise.recover
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class BindTest extends AnyFlatSpec with Matchers {
  "Either.value" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Either[Nothing, Int] = Right(1)
    val left: Either[String, Int] = Left("error")

    val actual = Raise.either {
      val x = one.value
      val y = recover({ left.value }) { _ => 1 }
      x + y
    }

    actual should be(Right(2))
  }

  "Option.value" should "return the value for a Some and raise an error for a None" in {
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None

    val actual = Raise.option {
      val x = some.value
      val y = recover({ none.value }) { _ => 1 }
      x + y
    }

    actual should be(Some(2))
  }

  "Try.value" should "return the value if it is not an error and raise an Error otherwise" in {
    val one: Try[Int]     = Success(1)
    val failure: Try[Int] = Failure(new Exception("error"))

    val actual = Raise.asTry {
      val x = one.value
      val y = Raise.recover({ failure.value }) { _ => 1 }
      x + y
    }

    actual should be(Success(2))
  }

  "List[Either[Error, A]].value" should "return a value if the list doesn't contain errors and raise an error otherwise" in {
    val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    val listWithError: List[Either[String, Int]] = List(Right(1), Left("error"), Right(3))

    val actual = Raise.either {
      val x = list.value
      val y = recover({ listWithError.value }) { _ => List(1) }
      x.sum + y.sum
    }

    actual should be(Right(7))
  }
}
