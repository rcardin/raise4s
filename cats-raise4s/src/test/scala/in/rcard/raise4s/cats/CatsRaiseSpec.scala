package in.rcard.raise4s.cats

import cats.Semigroup
import cats.data.{NonEmptyChain, NonEmptyList, Validated}
import in.rcard.raise4s.RaiseAnyPredef.raise
import in.rcard.raise4s.{Raise, raises}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CatsRaiseSpec extends AnyFlatSpec with Matchers {
  case class MyError2(errors: List[String])

  given Semigroup[MyError2] with {
    def combine(error1: MyError2, error2: MyError2): MyError2 =
      MyError2(error1.errors ++ error2.errors)
  }

  "mapOrAccumulate on Semigroup[Error]" should "map all the element of the iterable" in {
    val block: List[Int] raises MyError2 =
      CatsRaise.mapOrAccumulateS(List(1, 2, 3, 4, 5)) { value1 =>
        value1 + 1
      }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors using the combine function" in {
    val block: List[Int] raises MyError2 =
      CatsRaise.mapOrAccumulateS(List(1, 2, 3, 4, 5)) { value =>
        if (value % 2 == 0) {
          Raise.raise(MyError2(List(value.toString)))
        } else {
          value
        }
      }

    val actual = Raise.fold(
      block,
      identity,
      identity
    )

    actual shouldBe MyError2(List("2", "4"))
  }

  "mapOrAccumulateS on NonEmptyList" should "map all the element of the iterable" in {
    val block: NonEmptyList[Int] raises MyError2 =
      CatsRaise.mapOrAccumulateS(NonEmptyList.of(1, 2, 3, 4, 5)) { value1 =>
        value1 + 1
      }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe NonEmptyList.of(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors using the combine function" in {
    val block: NonEmptyList[Int] raises MyError2 =
      CatsRaise.mapOrAccumulateS(NonEmptyList.of(1, 2, 3, 4, 5)) { value =>
        if (value % 2 == 0) {
          Raise.raise(MyError2(List(value.toString)))
        } else {
          value
        }
      }

    val actual = Raise.fold(
      block,
      identity,
      identity
    )

    actual shouldBe MyError2(List("2", "4"))
  }

  "mapOrAccumulate to NonEmptyList" should "map all the element of the iterable" in {
    val block: List[Int] raises NonEmptyList[String] =
      CatsRaise.mapOrAccumulate(List(1, 2, 3, 4, 5)) { value1 =>
        value1 + 1
      }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors" in {
    val block: List[Int] raises NonEmptyList[String] =
      CatsRaise.mapOrAccumulate(List(1, 2, 3, 4, 5)) { value =>
        if (value % 2 == 0) {
          Raise.raise(value.toString)
        } else {
          value
        }
      }

    val actual = Raise.fold(
      block,
      identity,
      identity
    )

    actual shouldBe NonEmptyList.of("2", "4")
  }

  "mapOrAccumulate on a NonEmptyList to NonEmptyList" should "map all the element of the iterable" in {
    val block: NonEmptyList[Int] raises NonEmptyList[String] =
      CatsRaise.mapOrAccumulate(NonEmptyList.of(1, 2, 3, 4, 5)) { value1 =>
        value1 + 1
      }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe NonEmptyList.of(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors" in {
    val block: NonEmptyList[Int] raises NonEmptyList[String] =
      CatsRaise.mapOrAccumulate(NonEmptyList.of(1, 2, 3, 4, 5)) { value =>
        if (value % 2 == 0) {
          Raise.raise(value.toString)
        } else {
          value
        }
      }

    val actual = Raise.fold(
      block,
      identity,
      identity
    )

    actual shouldBe NonEmptyList.of("2", "4")
  }

  "zipOrAccumulateS with combine" should "zip 9 elements" in {
    val block: List[Int] raises MyError2 =
      CatsRaise.zipOrAccumulateS({ 1 }, { 2 }, { 3 }, { 4 }, { 5 }, { 6 }, { 7 }, { 8 }, { 9 }) {
        case (a, b, c, d, e, f, g, h, i) =>
          List(a, b, c, d, e, f, g, h, i)
      }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  it should "accumulate errors combining them" in {
    val block: List[Int] raises MyError2 = CatsRaise.zipOrAccumulateS(
      { 1 },
      { if (true) Raise.raise(MyError2(List("2"))) else 2 },
      { 3 },
      { if (true) Raise.raise(MyError2(List("4"))) else 4 },
      { 5 },
      { if (true) Raise.raise(MyError2(List("6"))) else 6 },
      { 7 },
      { if (true) Raise.raise(MyError2(List("8"))) else 8 },
      { 9 }
    ) { case (a, b, c, d, e, f, g, h, i) =>
      List(a, b, c, d, e, f, g, h, i)
    }

    val actual = Raise.fold(
      block,
      identity,
      identity
    )

    actual should be(MyError2(List("2", "4", "6", "8")))
  }

  "zipOrAccumulate on NonEmptyList" should "zip 9 elements" in {
    val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
      { 1 },
      { 2 },
      { 3 },
      { 4 },
      { 5 },
      { 6 },
      { 7 },
      { 8 },
      { 9 }
    ) { case (a, b, c, d, e, f, g, h, i) =>
      List(a, b, c, d, e, f, g, h, i)
    }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  it should "accumulate errors" in {
    val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
      { 1 },
      { if (true) Raise.raise("2") else 2 },
      { 3 },
      { if (true) Raise.raise("4") else 4 },
      { 5 },
      { if (true) Raise.raise("6") else 6 },
      { 7 },
      { if (true) Raise.raise("8") else 8 },
      { 9 }
    ) { case (a, b, c, d, e, f, g, h, i) =>
      List(a, b, c, d, e, f, g, h, i)
    }

    val actual = Raise.fold(
      block,
      identity,
      identity
    )

    actual shouldBe NonEmptyList.of("2", "4", "6", "8")
  }

  "The 'validated' builder" should "create a Valid instance" in {
    CatsRaise.validated({ 42 }) should be(Validated.valid(42))
  }

  it should "create an Invalid instance" in {
    CatsRaise.validated { raise("error") } should be(Validated.invalid("error"))
  }

  "The 'validatedNec' builder" should "create a Valid instance" in {
    CatsRaise.validatedNec {
      42
    } should be(Validated.valid(42))
  }

  it should "create an Invalid instance" in {
    CatsRaise.validatedNec {
      raise("error")
    } should be(Validated.invalid(NonEmptyChain.one("error")))
  }

  "The 'validatedNel' builder" should "create a Valid instance" in {
    CatsRaise.validatedNel {
      42
    } should be(Validated.valid(42))
  }

  it should "create an Invalid instance" in {
    CatsRaise.validatedNel {
      raise("error")
    } should be(Validated.invalid(NonEmptyList.one("error")))
  }
}
