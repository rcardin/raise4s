![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/rcardin/raise4s/scala.yml?branch=main)
![Maven Central](https://img.shields.io/maven-central/v/in.rcard/raise4s_3)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/rcardin/raise4s)

# raise4s
Porting of the Raise DSL from the Arrow Kt Kotlin library

## Dependency

The library is available on Maven Central. To use it, add the following dependency to your `build.sbt` files:

```sbt
libraryDependencies += "in.rcard" % "raise4s_3" % "0.0.1"
```

The library is only available for Scala 3.

## Usage

The Raise DSL is a new way to handle typed errors in Scala. Instead of using a wrapper type to address both the happy path and errors, the `Raise[E]` type describes the possibility that a function can raise a logical error of type `E`. A function that can raise an error of type `E` must execute in a scope that can also handle the error. In recent Scala, it's something that is referred to _direct style_.

The easiest way to define a function that can raise an error of type `E` is to create a context function using the `Raise[E]` the implicit parameter:

```scala 3
case class User(id: String)

sealed trait Error
case class UserNotFound(id: String) extends Error

def findUserById(id: String): Raise[Error] ?=> User = User(id)
```

We can do better than that, using the `infix type raises`:

```scala 3
def findUserById(id: String): User raises Error = User(id)
```

How do we read the above syntax? The function `findUserById` returns a `User` and can raise an error of type `String`.

The above function let us short-circuit an execution and raise an error of type `String` using the `raise` function:

```scala 3
def findUserById(id: String): User raises Error =
  if (id == "42") User(id) else raise(UserNotFound(id))
```

The type of error a function can raise is checked at compile time. If we try to raise an error of a different type, the compiler will complain:

```scala 3
def findUserById(id: String): User raises Error =
  if (id == "42") User(id) else raise("User not found")
```

The above code will not compile with the following error:

```
[error] 9 |  if (id == "42") User(id) else raise("User not found")
[error]   |                                                       ^
[error]   |No given instance of type in.rcard.raise4s.Raise[String] was found for parameter raise of method raise in package in.rcard.raise4s
[error] one error found
```

We may have noticed that one advantage of using the `Raise[E]` context is that the return type of the function listed only the happy path. As we’ll see in a moment, this is a huge advantage when we want to compose functions that can raise errors.

As you might guess from the previous compiler error, the Raise DSL is using implicit resolutions under the hood. In fact, to execute a function that uses the Raise DSL we need to provide an instance of the `Raise[E]` type class for the error type `E`. The most generic way to execute a function that can raise an error of type `E` and that is defined in the context of a `Raise[E]` is the `fold` function:

```scala 3
fold(
  block = { findUserById("43") },
  catchBlock = ex => println(s"Error: $ex"),
  recover = error => println(s"User not found: $error"),
  transform = user => println(s"User found: $user")
)
```

Let’s split the above function into parts. The `block` parameter is the function that we want to execute. The `catchBlock` parameter is a function that is executed when the `block` function throws an exception. Don't worry: The lambda handles only `NonFatal` exceptions. The `recover` parameter is a function that is executed when the `block` function raises a logical typed error of type `E`. Finally, the `transform` parameter is a function that is executed when the block function returns a value of type `A`, which is the happy path. All the handling blocks return the exact value of type `B`.

The `fold` function “consumes” the context, creating a concrete instance of a `Raise[E]` type and executing the `block` lambda in the context of that instance.

There are other flavors of the `fold` function. So, please, be sure to check them in the documentation.

Please be aware that any exception thrown inside the `Raise[E]` context will bubble up and not be transformed automatically into a logical typed error. What if we want to convert the exception into a typed error? For example, we want to convert the `IllegalArgumentException` into a `UserNotFound`. Well, we can do it using a function called `$catch`:

```scala 3
def findUserByIdWithEx(id: String): User =
  if (id == "42") User(id) else throw new IllegalArgumentException(s"User not found with id: $id")

val maybeUser: Either[Error, User] =
  either:
    $catch[User](() => findUserByIdWithEx("42"), {
      case _: IllegalArgumentException => raise(UserNotFound("42"))
    })
```

We will see the `either` function in a moment. As we can see, there’s nothing special with the `$catch` function. It just catches the exception and calls the catch lambda with the exception. The `$catch` function lets the fatal exception bubble up.





