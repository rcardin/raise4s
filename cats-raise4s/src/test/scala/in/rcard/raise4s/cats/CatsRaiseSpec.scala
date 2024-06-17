package in.rcard.raise4s.cats

import cats.Semigroup
import in.rcard.raise4s.{Raise, raises}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CatsRaiseSpec extends AnyFlatSpec with Matchers {
  case class MyError2(errors: List[String])

  given Semigroup[MyError2] with {
    def combine(error1: MyError2, error2: MyError2): MyError2 =
      MyError2(error1.errors ++ error2.errors)
  }
  
  "mapOrAccumulate with combine function" should "map all the element of the iterable" in {
    val block: List[Int] raises MyError2 =
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

  it should "accumulate all the errors using the combine function" in {
    val block: List[Int] raises MyError2 =
      CatsRaise.mapOrAccumulate(List(1, 2, 3, 4, 5)) { value =>
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
}
