package in.rcard.raise4s.cats

import cats.data.*
import in.rcard.raise4s.cats.CatsBind.{value, values, combineErrorsS}
import in.rcard.raise4s.{Raise, raises, Raise as value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CatsBindSpec extends AnyFlatSpec with Matchers {

  "Validated.value" should "return the value if it's valid" in {

    val one: Validated[String, Int] = Validated.Valid(1)

    val actual: Int = Raise.recover(one.value) { _ => 2 }

    actual should be(1)
  }

  it should "raise an error if it's invalid" in {

    val invalid: Validated[String, Int] = Validated.Invalid("error")

    val actual: Int = Raise.recover(invalid.value) { err => if (err == "error") 2 else 3 }

    actual should be(2)
  }

  it should "work with invalid ValidatedNel" in {

    val invalid: ValidatedNel[String, Int] = Validated.invalid(NonEmptyList.one("error"))

    val actual: Int = Raise.recover(invalid.value) { err => if (err.head == "error") 2 else 3 }

    actual should be(2)
  }

  it should "work with invalid ValidatedNec" in {

    val invalid: ValidatedNec[String, Int] = Validated.invalid(NonEmptyChain.one("error"))

    val actual: Int = Raise.recover(invalid.value) { err => if (err.head == "error") 2 else 3 }

    actual should be(2)
  }

  "values extension function" should "map all the element of the iterable" in {
    val iterableWithInnerRaise: List[Int raises String] = List(1, 2, 3, 4, 5).map { value1 =>
      value1 + 1
    }

    val iterableWithOuterRaise: List[Int] raises NonEmptyList[String] =
      iterableWithInnerRaise.values

    val actual = Raise.fold(
      iterableWithOuterRaise,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors" in {
    val iterableWithInnerRaise: List[Int raises String] =
      List(1, 2, 3, 4, 5).map { value =>
        if (value % 2 == 0) {
          Raise.raise(value.toString)
        } else {
          value
        }
      }

    val iterableWithOuterRaise: List[Int] raises NonEmptyList[String] =
      iterableWithInnerRaise.values

    val actual = Raise.fold(
      iterableWithOuterRaise,
      identity,
      identity
    )

    actual shouldBe NonEmptyList.of("2", "4")
  }

  "values extension function on non-empty list" should "map all the element of the non-empty list" in {
    val nonEmptyListWithInnerRaise: NonEmptyList[Int raises String] = NonEmptyList.of(1, 2, 3, 4, 5).map { value1 =>
      value1 + 1
    }

    val nonEmptyWithOuterRaise: NonEmptyList[Int] raises NonEmptyList[String] =
      nonEmptyListWithInnerRaise.values

    val actual = Raise.fold(
      nonEmptyWithOuterRaise,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe NonEmptyList.of(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors" in {
    val nonEmptyListWithInnerRaise: NonEmptyList[Int raises String] =
      NonEmptyList.of(1, 2, 3, 4, 5).map { value =>
        if (value % 2 == 0) {
          Raise.raise(value.toString)
        } else {
          value
        }
      }

    val nonEmptyWithOuterRaise: NonEmptyList[Int] raises NonEmptyList[String] =
      nonEmptyListWithInnerRaise.values

    val actual = Raise.fold(
      nonEmptyWithOuterRaise,
      identity,
      identity
    )

    actual shouldBe NonEmptyList.of("2", "4")
  }

  "combineErrorS on Semigroup[Error]" should "map all the element of the iterable" in {
    val iterableWithInnerRaise: List[Int raises String] =
      List(1, 2, 3, 4, 5).map { value1 =>
        value1 + 1
      }

    val iterableWithOuterRaise: List[Int] raises String = iterableWithInnerRaise.combineErrorsS

    val actual = Raise.fold(
      iterableWithOuterRaise,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors using the combine function" in {
    val iterableWithInnerRaise: List[Int raises String] =
      List(1, 2, 3, 4, 5).map { value =>
        if (value % 2 == 0) {
          Raise.raise(value.toString)
        } else {
          value
        }
      }

    val iterableWithOuterRaise: List[Int] raises String = iterableWithInnerRaise.combineErrorsS

    val actual = Raise.fold(
      iterableWithOuterRaise,
      identity,
      identity
    )

    actual shouldBe "24"
  }
}
