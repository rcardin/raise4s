package in.rcard.raise4s

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RaiseSpec extends AnyFlatSpec with Matchers {

  "ensure" should "return Unit if the given condition is met" in {
    val actual: Int = fold(
      () => ensure(42 > 0, () => "error"),
      error => 43,
      value => 42
    )
    actual should be(42)
  }

  it should "return the error if the condition is not met" in {
    val actual: Int = fold(
      () => ensure(42 < 0, () => "error"),
      error => 43,
      value => 42
    )
    actual should be(43)
  }

  "ensureNotNull" should "return the value if it is not null" in {
    val actual: Int = fold(
      () => ensureNotNull(42, () => "error"),
      error => 43,
      identity
    )
    actual should be(42)
  }

  it should "return the error if the value is null" in {
    val actual: Int = fold(
      () => ensureNotNull(null, () => "error"),
      error => 43,
      value => 42
    )
    actual should be(43)
  }

  "recover" should "return the value if it is not an error" in {
    val actual = recover(
      () => 42,
      error => 43
    )

    actual should be(42)
  }

  it should "return the recovery value if the value is an error" in {
    val actual = recover(
      () => raise("error"),
      error => 43
    )

    actual should be(43)
  }

  it should "rethrow the exception" in {
    assertThrows[RuntimeException] {
      recover(
        () => throw new RuntimeException("error"),
        error => 43
      )
    }
  }

  "recover with catchBlock" should "return the value if it is not an error" in {
    val actual = recover(
      () => 42,
      error => 43,
      ex => 44
    )

    actual should be(42)
  }

  it should "return the recovery value if the value is an error" in {
    val actual = recover(
      () => raise("error"),
      error => 43,
      ex => 44
    )

    actual should be(43)
  }

  it should "return the recovery value if the value is an exception" in {
    val actual = recover(
      () => throw new RuntimeException("error"),
      error => 43,
      ex => 44
    )

    actual should be(44)
  }

  "$catch" should "return the value if no exception is thrown" in {
    val actual = $catch(
      () => 42,
      ex => 43
    )

    actual should be(42)
  }

  it should "return the recovery value if an exception is thrown" in {
    val actual = $catch(
      () => throw new RuntimeException("error"),
      ex => 43
    )

    actual should be(43)
  }
}
