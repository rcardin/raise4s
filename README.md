![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/rcardin/raise4s/scala.yml?branch=main)
![Maven Central](https://img.shields.io/maven-central/v/in.rcard.raise4s/core_3)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/rcardin/raise4s)
[![javadoc](https://javadoc.io/badge2/in.rcard.raise4s/core_3/javadoc.svg)](https://javadoc.io/doc/in.rcard.raise4s/core_3)

# Raise4s

Initially, the library was a port of the Raise DSL from the Arrow Kt Kotlin library. Now, it has diverged from the original library and has become a new library, implementing the effect of failing with a typed error.

Available modules are:

- `core`: The Raise DSL for Scala 3.
- `cats-raise4s`: The Raise DSL for Scala 3 with some useful Cats data structures.
- `munit-raise4s`: MUnit integration for the Raise DSL.

(See the README.md files in the subdirectories for more information.)

## Dependency

The library is available on Maven Central. To use it, add the following dependency to your `build.sbt` files:

```sbt
libraryDependencies += "in.rcard.raise4s" %% "core" % "0.5.0"
```

The library is only available for Scala 3.

## Usage

### The `Raise` DSL in Scala

The Raise DSL is a new way to handle typed errors in Scala. Instead of using a wrapper type to address both the happy
path and errors, the `Raise[E]` type describes the possibility that a function can raise a logical error of type `E`. A
function that can raise an error of type `E` must execute in a scope that can also handle the error. In recent Scala,
it's something that is referred to _direct style_.

The easiest way to define a function that can raise an error of type `E` is to create a context function using
the `Raise[E]` the implicit parameter:

```scala 3
import in.rcard.raise4s.*

case class User(id: String, name: String)

sealed trait Error

case class UserNotFound(id: String) extends Error

def findUserById(id: String): Raise[Error] ?=> User = User(id, "Alice")
```

We can do better than that, using the `infix type raises`:

```scala 3
def findUserById(id: String): User raises Error = User(id, "Alice")
```

How do we read the above syntax? The function `findUserById` returns a `User` and can raise an error of type `Error` or any of its subtypes.

The above function let us short-circuit an execution and raise an error of type `UserNotFound` using the `raise` function:

```scala 3
def findUserById(id: String): User raises Error =
  if (id == "42") User(id, "Alice") else Raise.raise(UserNotFound(id))
```

The type of error a function can raise is checked at compile time. If we try to raise an error of a different type, the
compiler will complain:

```scala 3
def findUserById(id: String): User raises Error =
  if (id == "42") User(id, "Alice") else Raise.raise("User not found")
```

The above code will not compile with the following error:

```
[error] 9 |  if (id == "42") User(id) else Raise.raise("User not found")
[error]   |                                                       ^
[error]   |No given instance of type in.rcard.raise4s.Raise[String] was found for parameter raise of method raise in package in.rcard.raise4s
[error] one error found
```

It's also possible to lift a value to the context `Raise[E] ?=> A`. If we want to lift a value of type `A` to the
context `Raise[Nothing]`, we can use the `succeed` function:

```scala 3
def aliceUser: User raises UserNotFound = Raise.succeed(User("42", "Alice"))
```

More useful is to lift a value of type `E` as the error in the context `Raise[E]`:

```scala 3
def userNotFound: User raises UserNotFound = UserNotFound("42").raise[User]
```

We can always rewrite che last function as follows:

```scala 3
def userNotFound: User raises UserNotFound = {
  Raise.raise(UserNotFound("42"))
}
```

We may have noticed that one advantage of using the `Raise[E]` context is that the return type of the function listed
only the happy path. As we’ll see in a moment, this is a huge advantage when we want to compose functions that can raise
errors.

As you might guess from the previous compiler error, the Raise DSL is using implicit resolutions under the hood. In
fact, to execute a function that uses the Raise DSL we need to provide an instance of the `Raise[E]` type class for the
error type `E`. The most generic way to execute a function that can raise an error of type `E` and that is defined in
the context of a `Raise[E]` is the `fold` function:

```scala 3
Raise.fold(
  block = {
    findUserById("43")
  },
  catchBlock = ex => println(s"Error: $ex"),
  recover = error => println(s"User not found: $error"),
  transform = user => println(s"User found: $user")
)
```

Let’s split the above function into parts. The `block` parameter is the function that we want to execute.
The `catchBlock` parameter is a function that is executed when the `block` function throws an exception. Don't worry:
The lambda handles only `NonFatal` exceptions. The `recover` parameter is a function that is executed when the `block`
function raises a logical typed error of type `E`. Finally, the `transform` parameter is a function that is executed
when the block function returns a value of type `A`, which is the happy path. All the handling blocks return the exact
value of type `B`.

The `fold` function “consumes” the context, creating a concrete instance of a `Raise[E]` type and executing the `block`
lambda in the context of that instance.

There are other flavors of the `fold` function. So, please, be sure to check them in the documentation.

For those who are not interested in handling possible exceptions raised by a function, there is a more straightforward
function available, called `run`:

```scala 3
val maybeUser: Error | User = Raise.run {
  findUserById("42")
}
```

Please be aware that any exception thrown inside the `Raise[E]` context will bubble up and not be transformed
automatically into a logical typed error. What if we want to convert the exception into a typed error? For example, we
want to convert the `IllegalArgumentException` into a `UserNotFound`. Well, we can do it using a function
called `catching`:

```scala 3
def findUserByIdWithEx(id: String): User =
  if (id == "42") User(id, "Alice") else throw new IllegalArgumentException(s"User not found with id: $id")

val maybeUser: Either[Error, User] =
  Raise.either:
    Raise.catching[User](() => findUserByIdWithEx("42")) {
      case _: IllegalArgumentException => Raise.raise(UserNotFound("42"))
    }
```

There is also a version of the `catching` function defined as an extension method of any `A` type. The above code can be
rewritten as follows:

```scala 3
findUserByIdWithEx("42").catching {
  case _: IllegalArgumentException => Raise.raise(UserNotFound("42"))
}
```

We will see the `either` function in a moment. As we can see, there’s nothing special with the `catching` function. It
just catches the exception and calls the catch lambda with the exception. The `catching` function lets the fatal
exception bubble up.

It’s a different story if we want to recover or react to a typed error. In this case, we can use the `recover` function:

```scala 3
case class NegativeAmount(amount: Double) extends Error

def convertToUsd(amount: Double, currency: String): Double raises NegativeAmount =
  if (amount < 0) Raise.raise(NegativeAmount(amount))
  else amount * 1.2

val usdAmount: Double =
  Raise.recover({
    convertToUsd(-1, "EUR")
  }) { case NegativeAmount(amount) => 0.0D }
```

If you don't care about the error type, and you want to recover from any error with a fixed value, you can use the `withDefault` function:

```scala 3
val usdAmount: Double =
  Raise.withDefault(0.0D) {
    convertToUsd(-1, "EUR")
  }
```

### Accumulating Errors

What if we want to accumulate more than one error in a dedicated data structure? For example, say we have a list of ids,
and we want to retrieve all the associated users.

```scala 3
def findUsersByIds(ids: List[String]): List[User] raises List[UserNotFound]
```

As you might guess, the library gives us a dedicated function to execute a transformation on a list of values and
accumulate the errors in a List[Error]. The function is called `mapOrAccumulate`:

```scala 3
def findUsersByIds(ids: List[String]): List[User] raises List[UserNotFound] =
  Raise.mapOrAccumulate(ids) { id =>
    findUserById(id)
  }
```

If at least one error is raised, the `mapOrAccumulate` function will accumulate all the errors in a List[Error]. If no
error is raised, the function will return a List[User]. There is also a version of the `mapOrAccumulate` function
defined as extension method of any `Iterable[A]` type:

```scala 3
import in.rcard.raise4s.RaiseIterableDef.mapOrAccumulate

def findUsersByIds(ids: List[String]): List[User] raises List[UserNotFound] =
  ids.mapOrAccumulate { id =>
    findUserById(id)
  }
```

We can obtain the same result using the `values` extension function:

```scala 3
import in.rcard.raise4s.RaiseIterableDef.values

def findUsersByIds(ids: List[String]): List[User] raises List[UserNotFound] =
  val usersOrErrors: List[User raises UserNotFound] = ids.map(id => findUserById(id))
  usersOrErrors.values
```

Did anyone say `traverse`?

### Zipping Errors

As we said, the `mapOrAccumulate` function allows the combination of the results of a transformation applied to a
collection of elements of the same type. What if we want to combine transformations applied to objects of different
types?

A classic example is the validation during the creation of an object. Say we want a `Salary` type with amount and
currency information:

```scala 3
case class Salary(amount: Double, currency: String)
```

Now, we need to create a hierarchy of the possible logical typed errors we can have while creating a Salary object.
We’ll check for the following two errors:

1. The amount must be greater than zero
2. The currency must be made of three capital letters

We define the following hierarchy of types to represent the above errors:

```scala 3
sealed trait SalaryError

case object NegativeAmount extends SalaryError

case class InvalidCurrency(message: String) extends SalaryError
```

In general, we want to avoid the creation of invalid objects. To do so, we can define what we call a smart constructor.
Smart constructors are factories that look like regular constructors but perform validations and generally return the
valid object or some typed error. The smart constructor must perform all the needed validation on input data before
creating a concrete instance of the object.

We can’t use the `mapOrAccumulate` function we previously saw because we don’t have a list of objects of the same type
as input. Fortunately, the library provides the `zipOrAccumulate` function, which we need.

```scala 3
object Salary {
  def apply(amount: Double, currency: String): Salary raises List[SalaryError] = {
    Raise.zipOrAccumulate(
      {
        Raise.ensure(amount >= 0.0)(NegativeAmount)
      },
      {
        Raise.ensure(currency != null && currency.matches("[A-Z]{3}")) {
          InvalidCurrency("Currency must be not empty and valid")
        }
      }
    ) { (_, _) =>
      Salary(amount, currency)
    }
  }
}
```

Many different versions of the function differ in the number of input parameters. The maximum number of single input
parameters is 9. If we need more, we must apply the function recursively multiple times.

### The New `accumulate` DSL

Recently, we added an experimental DSL for error accumulation inspired by the [new Arrow 2.0 library](https://arrow-kt.io/community/blog/2024/12/05/arrow-2-0/#simple-accumulation-in-raise). We decided to try a more user-friendly DSL instead of the exoteric `mapOrAccumulate` and `zipAccumulate` functions, called `accumulate`:

```scala 3
object Salary {
  def apply(amount: Double, currency: String): Salary raises List[SalaryError] = 
    accumulate {
      val validatedAmount = accumulating {
        ensure[SalaryError](amount >= 0.0)(NegativeAmount)
        amount
      }
      val validatedCurrency = accumulating {
        ensure[SalaryError](currency != null && currency.matches("[A-Z]{3}")) {
          InvalidCurrency("Currency must be not empty and valid")
        }
        currency
      }
      Salary(validatedAmount, validatedCurrency)
    }
}
```

The `accumulate` and `accumulating` functions are defined in the `in.rcard.raise4s.Accumulation` object. As you can see, no more lambda tuples are needed. Every raised error is intercepted and accumulated in a list of errors inside the `accumulating` block. The `accumulate` function will return the happy path or a list of errors.

The `accumulating` function returns an instance of the `Value[A]` type and not an instance of the validated `A` object itself. The first time the `Value[A]` instance is used, the library performs an implicit conversion to the `A` object under the hood. The implicit conversion will raise the accumulated errors if there is an error during the validation.

We can also use the `accumulate` API to mimic the behavior of the `mapOrAccumulate` function. For example, we can rewrite the `findUsersByIds` function as follows:

```scala 3
import in.rcard.raise4s.Accumulation.*

def findUsersByIds(ids: List[String]): List[User] raises List[Error] = accumulate {
  val users = ids.map(id => accumulating { findUserById(id) })
  users
}
```

The above code uses another implicit conversion under the hood. It's converting from a `List[Value[Error, User]]` to a `List[User] raises List[Error]`. In case you want to remove the extra `users` variable, you need to help the compiler understand the return type of the `map` function:

```scala 3
def findUsersByIds(ids: List[String]): List[User] raises List[Error] = accumulate {
  ids.map[Value[Error, User]](id => accumulating { findUserById(id) })
}
```

If we don't force the `map` function to return a `Value[Error, User]` instance, the compiler will infer the return type as `List[User]` and the implicit conversion for the single `Value[Error, User]` will be applied, instead of the one for the `List[Value[Error, User]]`.

### Conversion to Wrapped Types

What if we want to convert a computation in the `Raise[E]` context to a function returning an `Either[E, A]`,
a `Try[A]`, an `Option[A]`? Well, nothing is more straightforward than that.

Let’s start with `Either[E, A]`. The `either` builder is what we're searching for. We can translate the result of
the `findUserById` function to an `Either[Error, User]` quite easily:

```scala 3
val maybeUser: Either[Error, User] =
  Raise.either:
    findUserById("42")
```

If we want to retrieve more information of a user using her name, we can just use the `User` instance directly:

```scala 3
val maybeUserNameInUpperCase: Either[Error, String] =
  Raise.either:
    val user: User = findUserById("42")
    user.name.toUpperCase
```

Please praise the simplicity and absence of boilerplate code, like calls to `map` functions or when expressions. This is
the power of Scala direct style.

It’s also possible to make the backward conversion from an `Either[E, A]` to a `Raise[E]` using the `value` function:

```scala 3
val userNameInUpperCaseRaiseLambda: Raise[Error] ?=> String = maybeUserNameInUpperCase.value
```

The `value` function is very handful when we need to compose functions that return an `Either[E, A]`:

```scala 3
val one = Right(1)
val two = Right(2)
val three = Raise.either {
  val oneValue = one.value
  val twoValue = two.value
  oneValue + twoValue
}
```

The `value` function calls the `raise` function if the `Either` instance is a `Left`; otherwise, it returns the value
wrapped by the `Right` instance. Despite the trivial logic implemented in the above example, it's a good example of how
to compose functions that return an `Either[E, A]` using the Raise DSL without the use of any `flatMap` function.

A useful shortcut is available when we need to transform a `List[Either[E, A]]` into a `List[A] raises E`. The eventual
raised error `E` is the first error found in the list of `Either[E, A]`:

```scala 3
val eitherList: List[Either[String, Int]] = List(Right(1), Left("error"), Right(3))
val raiseList: List[Int] raises String = listWithError.value
```

Be aware that before version 0.0.5, the `value` function was called `bind()`.

We can do the same with `Try[A]` and `Option[A]` using the `asTry` and `option` builders, respectively. Let's start with
the `asTry` builder. In this case, the only available type of error is `Throwable`:

```scala 3
val maybeUserWithTry: Try[User] =
  Raise.asTry:
    findUserByIdWithEx("42")
```

As you might guess, any fatal exception thrown inside the `asTry` context will bubble up and not handled.

Last but not least, the `option` builder:

```scala 3
def findUserByIdWithNone(id: String): User raises None.type =
  if (id == "42") User(id, "Alice") else Raise.raise(None)

val maybeUserWithOpt: Option[User] =
  Raise.option:
    findUserByIdWithNone("42")
```

The `bind` function is available for `Try[A]` and `Option[A]` as well.

By the way, there are more feature in the Raise DSL. Please, check the documentation for more information.

## Strategies

We can call a _strategy_ the way we want to handle the errors raised by a function. The whole `raise4s` library give you
many predefined ways to deal with errors. For example, you can `fold` on a function, `run` it, `either` it, `asTry`
it, `option` it, and so on.

However, the library gives you the possibility to define your own strategy. To do so, you need to define a new instance
of the `Raise[E]` type class. The `Raise[E]` type class is a type class that defines how to handle errors of type `E`.
The `Raise[E]` type class is defined as follows:

```scala 3
trait Raise[-Error]:
  def raise(e: Error): Nothing
```

For example, you can define a strategy that simply throw an exception for every error raised by a function:

```scala 3
def loadConfiguration(file: String): Configuration raises ConfigurationError = ???

given Raise[Any] = error => throw new RuntimeException(error.toString)
val configuration = loadConfiguration(args(0))
```

The library gives you a type alias to define strategies on all the errors:

```scala 3
type anyRaised = Raise[Any]
```

Then, we can rewrite the above example as follows:

```scala 3
given anyRaised = error => throw new RuntimeException(error.toString)
```

We can define a strategy for a single error as well:

```scala 3
given Raise[ConfigurationError] = error => exit(1)
```

In the above example we're saying that a configuration error must stop the execution of the program.

### `MapError` Strategy

The library defines a `withError` function to map an error raised by a function to an error of a different type:

```scala 3
val actual = either {
  Raise.withError[Int, String, Int](s => s.length) {
    raise("error")
  }
}
actual should be(Left(5))
```

In the above example, we map the error of type `String` raised by the given lambda into an error of type `Int`.
The `withError` function does its job quite well. However, the function's ergonomics are not so good due to the pair of
lambdas we need to provide: The first one is the function that maps the error, and the second one is the function that
raises the error.

We can achieve the same result using a dedicated strategy. In detail, the library defines a `MapError` strategy that
maps an error of type `E` into an error of type `F`. The `MapError` strategy is defined as follows:

```scala 3
trait MapError[From, To] extends Raise[From] {
  def map(error: From): To

  def raise(error: From): Nothing = throw Raised(map(error))
}
```

Then, we can map an error of type `String` into an error of type `Int` by implementing the `MapError` strategy as
follows:

```scala 3
val finalLambda: String raises Int = {
  given MapError[String, Int] = error => error.length

  raise("Oops!")
}
val result: Int | String = Raise.run(finalLambda)
result shouldBe 5
```

As you can see, we're entirely focused on the happy path, and we can define how to handle the errors in a dedicated
place.

### `RecoverWith` Strategy

We've already seen how to recover from a typed error using the `Raise.recover` function. The function works quite well,
but it's not so ergonomic. We need to provide two lambdas as input for the function. The first is the block we want to
execute, and the second is the lambda to apply in case of recovery.

We can obtain the same result by applying the `RecoverWith` strategy and by using the `recoverable` DSL:

```scala 3
case class NegativeAmount(amount: Double) extends Error

def convertToUsd(amount: Double, currency: String): Double raises NegativeAmount =
  if (amount < 0) Raise.raise(NegativeAmount(amount))
  else amount * 1.2

given RecoverWith[NegativeAmount, Double] = {
  case NegativeAmount => 0.0D
}
val usdAmount: Double = Raise.recoverable {
  convertToUsd(-1, "EUR")
}
```

The strategy lets us define how to recover from a typed error in a dedicated place, leaving the happy logic in the main
block.

### Tracing

When a logical error is raised, the library will not print any information about the error by default. Sometimes, it's useful for debugging purposes to have a trace of the error. The library allows you to define a strategy called `TraceWith` that processes the error together with a stack trace that identifies where the error was raised.

To enable tracing for a specific code block, wrap it inside the `trace` DSL and provide a `TraceWith` strategy:

```scala 3
import in.rcard.raise4s.Raise.{raise, traced}
import in.rcard.raise4s.Strategies.TraceWith

given TraceWith[String] = (trace: Traced) => {
  trace.printStackTrace()
}
val lambda: Int raises String = traced {
  raise("Oops!")
}
val actual: String | Int = Raise.run(lambda)
actual shouldBe "Oops!"
```

The `Traced` exception the tracing engine uses contains the original typed error.

## Contributing

If you want to contribute to the project, please do it! Any help is welcome.

## Acknowledgments

This project is inspired by the Arrow Kt library's Raise DSL. I want to thank the Arrow Kt team for their outstanding
work. In detail, thanks to Simon Vergauwen for the great discussions we had on Slack. A lot of thanks also to Daniel
Ciocîrlan, my mentor and friend.






