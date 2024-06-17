<a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge.svg" height="40px" align="right" alt="Cats friendly" /></a>
<br/>

# Raise4s for Cats
Integration of the Raise DSL with some useful Cats data structures.

## Dependency

The library is available on Maven Central. To use it, add the following dependency to your `build.sbt` files:

```sbt
libraryDependencies += "in.rcard.raise4s" %% "cats-raise4s" % "0.0.6"
```

The library is only available for Scala 3.

## Usage 

In general, the integration lets you use the _Cats_ type classes with the _Raise_ DSL. In detail:

- Use the `Semigroup` type class to combine errors in the `mapOrAccumlate` function.

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