package in.rcard.raise4s

import in.rcard.raise4s.Raise.catching
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RaiseSpec extends AnyFlatSpec with Matchers {

  "ensure" should "return Unit if the given condition is met" in {
    val actual: Int = Raise.fold(
      { Raise.ensure(42 > 0) { "error" } },
      error => 43,
      value => 42
    )
    actual should be(42)
  }

  it should "return the error if the condition is not met" in {
    val actual: Int = Raise.fold(
      { Raise.ensure(42 < 0) { "error" } },
      error => 43,
      value => 42
    )
    actual should be(43)
  }

  "ensureNotNull" should "return the value if it is not null" in {
    val actual: Int = Raise.fold(
      { Raise.ensureNotNull(42, () => "error") },
      error => 43,
      identity
    )
    actual should be(42)
  }

  it should "return the error if the value is null" in {
    val actual: Int = Raise.fold(
      { Raise.ensureNotNull(null, () => "error") },
      error => 43,
      value => 42
    )
    actual should be(43)
  }

  "recover" should "return the value if it is not an error" in {
    val actual = Raise.recover(
      { 42 },
      error => 43
    )

    actual should be(42)
  }

  it should "return the recovery value if the value is an error" in {
    val actual = Raise.recover(
      { Raise.raise("error") },
      error => 43
    )

    actual should be(43)
  }

  it should "rethrow the exception" in {
    assertThrows[RuntimeException] {
      Raise.recover(
        { throw new RuntimeException("error") },
        error => 43
      )
    }
  }

  "recover with catchBlock" should "return the value if it is not an error" in {
    val actual = Raise.recover(
      { 42 },
      error => 43,
      ex => 44
    )

    actual should be(42)
  }

  it should "return the recovery value if the value is an error" in {
    val actual = Raise.recover(
      { Raise.raise("error") },
      error => 43,
      ex => 44
    )

    actual should be(43)
  }

  it should "return the recovery value if the value is an exception" in {
    val actual = Raise.recover(
      { throw new RuntimeException("error") },
      error => 43,
      ex => 44
    )

    actual should be(44)
  }

  "catching" should "return the value if no exception is thrown" in {
    val actual = Raise.catching(
      () => 42,
      ex => 43
    )

    actual should be(42)
  }

  it should "return the recovery value if an exception is thrown" in {
    val actual = Raise.catching(
      () => throw new RuntimeException("error"),
      ex => 43
    )

    actual should be(43)
  }

  it should "rethrow any fatal exception" in {
    assertThrows[OutOfMemoryError] {
      Raise.catching(
        () => throw new OutOfMemoryError("error"),
        ex => 43
      )
    }
  }

  "withError" should "return the value if it is not an error" in {
    val actual = Raise.either {
      Raise.withError[Int, String, Int](s => s.length, { 42 })
    }

    actual should be(Right(42))
  }

  it should "return the transformed error if the value is an error" in {
    val actual = Raise.either {
      Raise.withError[Int, String, Int](s => s.length, { Raise.raise("error") })
    }

    actual should be(Left(5))
  }

  "catching as an extension method" should "return the value if no exception is thrown" in {
    val actual = 42.catching { ex =>
      43
    }

    actual should be(42)
  }

  it should "return the recovery value if an exception is thrown" in {
    val actual = { throw new RuntimeException("error") }.catching { ex =>
      43
    }

    actual should be(43)
  }

  it should "rethrow any fatal exception" in {
    assertThrows[OutOfMemoryError] {
      { throw new OutOfMemoryError("error") }.catching { ex =>
        43
      }
    }
  }
}
