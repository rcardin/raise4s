package in.rcard.raise4s

import in.rcard.raise4s.RaiseIterableDef.{combineErrors, mapOrAccumulate, values}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FoldSpec extends AnyFlatSpec with Matchers {

  trait Error
  case object MyError extends Error

  "The fold function with four parameters" should "transform the result of the given Raise function" in {
    val actual: String = Raise.fold(
      { 42 },
      throwable => "Error: " + throwable.getMessage,
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Value: 42"
  }

  it should "transform the result of the given Raise function when it throws an exception" in {
    val actual: String = Raise.fold(
      { throw new RuntimeException("Boom!") },
      throwable => "Error: " + throwable.getMessage,
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Error: Boom!"
  }

  it should "transform the result of the given Raise function when it returns an error" in {
    val actual: String = Raise.fold(
      { Raise.raise(MyError) },
      throwable => "Error: " + throwable.getMessage,
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Error: MyError"
  }

  it should "rethrows any fatal exception" in {
    assertThrows[OutOfMemoryError] {
      Raise.fold(
        { throw new OutOfMemoryError("Boom!") },
        throwable => "Error: " + throwable.getMessage,
        error => "Error: " + error,
        value => "Value: " + value
      )
    }
  }

  "The fold function without the 'catch' block " should "transform the result of the given Raise function" in {
    val actual: String = Raise.fold(
      { 42 },
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Value: 42"
  }

  it should "transform the result of the given Raise function when it returns an error" in {
    val actual: String = Raise.fold(
      { Raise.raise(MyError) },
      error => "Error: " + error,
      value => "Value: " + value
    )

    actual shouldBe "Error: MyError"
  }

  it should "rethrows any exception" in {
    assertThrows[RuntimeException] {
      Raise.fold(
        { throw new RuntimeException("Boom!") },
        error => "Error: " + error,
        value => "Value: " + value
      )
    }
  }

  "mapOrAccumulate" should "map all the element of the iterable" in {
    val block: List[Int] raises List[String] = Raise.mapOrAccumulate(List(1, 2, 3, 4, 5)) {
      value1 => value1 + 1
    }

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors" in {
    val block: List[Int] raises List[String] = Raise.mapOrAccumulate(List(1, 2, 3, 4, 5)) { value =>
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

    actual shouldBe List("2", "4")
  }

  "Values extension method" should "extract the values from the iterable" in {
    val iterableWithInnerRaise: List[Int raises String]     = List(1, 2, 3, 4, 5)
    val iterableWithRaiseAcc: List[Int] raises List[String] = iterableWithInnerRaise.values

    val actual = Raise.fold(
      iterableWithRaiseAcc,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(1, 2, 3, 4, 5)
  }

  it should "accumulate all the errors" in {
    val iterableWithInnerRaise: List[Int raises String] = List(1, 2, 3, 4, 5).map(value =>
      if (value % 2 == 0) {
        Raise.raise(value.toString)
      } else {
        value
      }
    )

    val iterableWithRaiseAcc: List[Int] raises List[String] = iterableWithInnerRaise.values

    val actual = Raise.fold(
      iterableWithRaiseAcc,
      identity,
      identity
    )

    actual shouldBe List("2", "4")
  }

  case class MyError2(errors: List[String])
  def combineErrors(error1: MyError2, error2: MyError2): MyError2 =
    MyError2(error1.errors ++ error2.errors)

  "mapOrAccumulate with combine function" should "map all the element of the iterable" in {
    val block: List[Int] raises MyError2 =
      Raise.mapOrAccumulate(List(1, 2, 3, 4, 5), combineErrors) { value1 =>
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
      Raise.mapOrAccumulate(List(1, 2, 3, 4, 5), combineErrors) { value =>
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

  "The extension function mapOrAccumulate" should "map all the element of the receiver" in {
    val block: List[Int] raises List[String] = List(1, 2, 3, 4, 5).mapOrAccumulate(_ + 1)

    val actual = Raise.fold(
      block,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(2, 3, 4, 5, 6)
  }

  it should "accumulate all the errors on the receiver" in {
    val block: List[Int] raises List[String] = List(1, 2, 3, 4, 5).mapOrAccumulate { value =>
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

    actual shouldBe List("2", "4")
  }

  "combineErrors extension function" should "map all the element of the iterable" in {
    val iterableWithInnerRaise: List[Int raises MyError2] = List(1, 2, 3, 4, 5)
    val iterableWithErrorsCombined: List[Int] raises MyError2 =
      iterableWithInnerRaise.combineErrors(combineErrors)

    val actual = Raise.fold(
      iterableWithErrorsCombined,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual shouldBe List(1, 2, 3, 4, 5)
  }

  it should "accumulate all the errors using the combine function" in {
    val iterableWithInnerRaise: List[Int raises MyError2] = List(1, 2, 3, 4, 5).map(value =>
      if (value % 2 == 0) {
        Raise.raise(MyError2(List(value.toString)))
      } else {
        value
      }
    )

    val iterableWithErrorsCombined: List[Int] raises MyError2 =
      iterableWithInnerRaise.combineErrors(combineErrors)

    val actual = Raise.fold(
      iterableWithErrorsCombined,
      identity,
      identity
    )

    actual shouldBe MyError2(List("2", "4"))
  }

  "zipOrAccumulate" should "zip 9 elements" in {
    val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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
    val block: List[Int] raises List[String] = Raise.zipOrAccumulate(
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

    actual should be(List("2", "4", "6", "8"))
  }

  "zipOrAccumulate with combine" should "zip 9 elements" in {
    val block: List[Int] raises MyError2 = Raise.zipOrAccumulate(combineErrors)(
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

  it should "accumulate errors combining them" in {
    val block: List[Int] raises MyError2 = Raise.zipOrAccumulate(combineErrors)(
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
}
