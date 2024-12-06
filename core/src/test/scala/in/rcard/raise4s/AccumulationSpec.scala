package in.rcard.raise4s

import in.rcard.raise4s.Accumulation.{accumulate, accumulating}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AccumulationSpec extends AnyFlatSpec with Matchers {

  "accumulate function" should "combine different values" in {

    val accumulationResult: List[Int] raises List[String] = accumulate {
      val a = accumulating { 1 }
      val b = accumulating { 2 }
      val c = accumulating { 3 }

      List(a, b, c)
    }

    val actual = Raise.fold(
      accumulationResult,
      error => fail(s"An error occurred: $error"),
      identity
    )

    actual should be(List(1, 2, 3))
  }

  it should "accumulate errors" in {
    val accumulationResult: List[Int] raises List[String] = accumulate {
      val a = accumulating { int(1) }
      val b = accumulating { int(2) }
      val c = accumulating { int(3) }

      List(a, b, c)
    }

    val actual = Raise.fold(
      accumulationResult,
      identity,
      identity
    )

    actual should be(List("2", "3"))
  }

  private def int(value: Int): Int raises String = {
    if value >= 2 then Raise.raise(value.toString)
    else value
  }
}
