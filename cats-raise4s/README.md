![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/rcardin/raise4s/scala.yml?branch=main)
![Maven Central](https://img.shields.io/maven-central/v/in.rcard.raise4s/cats-raise4s_3)
[![javadoc](https://javadoc.io/badge2/in.rcard.raise4s/cats-raise4s_3/javadoc.svg)](https://javadoc.io/doc/in.rcard.raise4s/cats-raise4s_3)
<a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge.svg" height="40px" align="right" alt="Cats friendly" /></a>
<br/>

# Raise4s for Cats

Integration of the Raise DSL with some useful Cats data structures.

## Dependency

The library is available on Maven Central. To use it, add the following dependency to your `build.sbt` files:

```sbt
libraryDependencies += "in.rcard.raise4s" %% "cats-raise4s" % "0.5.0"
```

The library is only available for Scala 3.

## Usage

In general, the integration lets you use the _Cats_ type classes with the _Raise_ DSL. In detail:

- Use the `Semigroup` type class to combine errors in the `mapOrAccumlateS` function.

  ```scala 3
  case class MyError2(errors: List[String])
  
  given Semigroup[MyError2] with {
    def combine(error1: MyError2, error2: MyError2): MyError2 =
      MyError2(error1.errors ++ error2.errors)
  }
  
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
  ```

- Use of the `NonEmptyList` data class to handle errors in the `mapOrAccumulate` function.

  ```scala 3
  val block: List[Int] raises NonEmptyList[String] = Raise.mapOrAccumulate(List(1, 2, 3, 4, 5)) {
    _ + 1
  }
  
  val actual = Raise.fold(
    block,
    error => fail(s"An error occurred: $error"),
    identity
  )
  
  actual shouldBe List(2, 3, 4, 5, 6)
  ```

- Use of the `NonEmptyList` data class to handle both the results and the errors in the `mapOrAccumulate` function.

  ```scala 3
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
  ```

- Use the `values` extension function to accumulate errors into a `NonEmptyList[E]` from a `Iterable[A raises E]` block.

  ```scala 3
  import in.rcard.raise4s.cats.CatsBind.values
  
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
  ```

- Use the `values` extension function to accumulate errors into a `NonEmptyList[E]` from a `NonEmptyList[A raises E]` block.

  ```scala 3
  import in.rcard.raise4s.cats.CatsBind.values
  
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
  ```

- Use the `combineErrorsS` with a `Semigroup` instance to accumulate errors into a `E` from a `Iterable[A raises E]` block.

  ```scala 3
  import cats.data.*
  import in.rcard.raise4s.cats.CatsBind.combineErrorsS

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
  ```

- Use the `combineErrorsS` with a `Semigroup` instance to accumulate errors into a `E` from a `NonEmptyList[A raises E]` block.

  ```scala 3
  import cats.data.*
  import in.rcard.raise4s.cats.CatsBind.combineErrorsS

  val iterableWithInnerRaise: NonEmptyList[Int raises String] =
    NonEmptyList.of(1, 2, 3, 4, 5).map { value =>
      if (value % 2 == 0) {
        Raise.raise(value.toString)
      } else {
        value
      }
    }

  val iterableWithOuterRaise: NonEmptyList[Int] raises String = iterableWithInnerRaise.combineErrorsS
  
  val actual = Raise.fold(
    iterableWithOuterRaise,
    identity,
    identity
  )
  
  actual shouldBe "24"
  ```

- Use the `Semigroup` type class to combine errors in the `zipOrAccumlateS` set of functions.

  ```scala 3
  case class MyError2(errors: List[String])
  given Semigroup[MyError2] with {
    def combine(error1: MyError2, error2: MyError2): MyError2 =
      MyError2(error1.errors ++ error2.errors)
  }
  val block: List[Int] raises MyError2 =
    CatsRaise.zipOrAccumulateS({ 1 }, { 2 }) {
      case (a, b) =>
        List(a, b)
    }
  val actual = Raise.fold(
    block,
    error => fail(s"An error occurred: $error"),
    identity
  )
  actual should be(List(1, 2))
  ```

- Use of the `NonEmptyList` data class to handle errors in the `zipOrAccumulate` function.

  ```scala 3
  val block: List[Int] raises NonEmptyList[String] = CatsRaise.zipOrAccumulate(
    { 1 },
    { 2 },
    { 3 },
    { 4 }
  ) { case (a, b) =>
    List(a, b)
  }
  val actual = Raise.fold(
    block,
    error => fail(s"An error occurred: $error"),
    identity
  )
  actual should be(List(1, 2))
  ```

- Convert a `Validated[Error, A]` instance to a `A raises Error` instance (it works with `ValidatedNel` and `ValidatedNec` either).

  ```scala 3
  val one: Validated[String, Int] = Validated.Valid(1)
  val actual: Int = Raise.recover(one.value) { _ => 2 }
  actual should be(1)
  ```

- Convert a `A raises Error` block into a `Validated[Error, A]` instance. The library has also builders `validatedNec` and `validatedNel` to convert into `ValidatedNec` and `ValidatedNel` instances.

  ```scala 3
  CatsRaise.validated { raise("error") } should be(Validated. invalid("error"))
  ```
  
- Use the `RaiseMonadError[E]` to integrate with your existing code that uses `MonadError`.

  ```scala 3
  import in.rcard.raise4s.cats.instances.raise.given
  
  private def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit
    ae: ApplicativeError[F, String]
  ): F[Int] = {
    if (y == 0) ae.raiseError("divisor is zero")
    else {
      ae.pure(x / y)
    }
  }
  
  type OrError[A] = Raise[String] ?=> A
  val actual: OrError[Int] = attemptDivideApplicativeError[OrError](30, 0)
  Raise.run { actual } shouldBe "divisor is zero"
```