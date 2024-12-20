![GitHub Workflow Status (with branch)](https://img.shields.io/github/actions/workflow/status/rcardin/raise4s/scala.yml?branch=main)
![Maven Central](https://img.shields.io/maven-central/v/in.rcard.raise4s/munit-raise4s_3)
[![javadoc](https://javadoc.io/badge2/in.rcard.raise4s/muint-raise4s_3/javadoc.svg)](https://javadoc.io/doc/in.rcard.raise4s/munit-raise4s_3)
<br/>

# MUnit Integration for Raise4s

Integration of the Raise DSL with the MUnit testing framework. 

## Dependency

The library is available on Maven Central. To use it, add the following dependency to your `build.sbt` files:

```sbt
libraryDependencies += "in.rcard.raise4s" %% "munit-raise4s" % "0.4.0"
```

The library is only available for Scala 3.

## Usage

The library let you test functions that can raise errors with the `munit` testing framework. Please refer to the [mUnit documentation](https://scalameta.org/munit/) for more general information about the testing framework.

To declare a new test, use the `testR` method available through the `RaiseSuite` test suite. The method accepts a body that can raise errors. For example:

```scala 3
import in.rcard.raise4s.munit.RaiseSuite

class RaiseSuiteSpec extends RaiseSuite {
  testR("A test should succeed on successful Raise instances") {
    val one: Int raises String = 1
    val two: Int raises String = 2
    assert(one < two)
  }
}
```

If the body raises an error, the test will fail with the error message. For example, the following test fails:

```scala 3
testR("A test should fail on failed Raise instances") {
  val one: Int raises String = Raise.raise("An error occurred")
  val two: Int raises String = 2
  assert(one < two)
}
```

The message we receive is:

```
munit.FailException: Expected the test not to raise any errors but it did with error 'An error occurred'
```

If you want to test if a block raises an error and you want to assert some properties on the error, you can use the `in.rcard.raise4s.munit.RaiseAssertions.interceptR` method:

```scala 3
testR("intercept assertion should succeed on a raised error") {
  val error: String = interceptR[String] {
    Raise.raise("An error occurred")
  }
  assertEquals(error, "An error occurred")
}
```

