# Type-safe off-heap memory for Scala

[![Join the chat at https://gitter.im/densh/scala-offheap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/densh/scala-offheap)


## Memory & regions

Scala-offheap introduces a concept of `Memory` which is an offpsring of ByteBuffers
and sun.misc.Unsafe. This part of API is intentionally left low-level as it's going to
be an underlying abstraction that higher-abstractions like offheap classes and arrays
are going to be desugared into.

We also introduce special kind of `Memory` called `Region` which denotes a specific
kind of growable memory api that implements efficient allocation & deallocation based
on memory pooling.

```scala
Region { r =>
  // perform arbitrary allocations here
}
```

## Offheap classes

Offheap classes come in two flavors: `@enum` classes and `@data` classes. Both of them
desugar down to a value classes over offheap references. This lets them both enjoy
low-overhead nature of the value classes but at the same time share some of their
deficiencies.

### @data class

`@data` classes are aimed to be an offheap equivalent of case classes. Apart from field
accessors (and possibly setters) they also generate automatic name-based pattern matching
support methods (`isEmpty`, `nonEmpty`, `get`, `_1`, ... , `_N`), `copy` method,
`toString` method and memory-safe casting primitives `is[T]` and `as[T]`.

```scala
  @data class Point(x: Int, y: Int)
```

To allocate a data object one needs to have an implicit memory in scope.

```scala
  implicit val memory = NativeMemory()
  val point = Point(10, 20)
```

Similarly to case classes one can have methods and type alias definitions inside them.
Due to restrictions of value classes, traits or objects in the body of @data class are not supported.

Another important distinction is the fact that comparison on offheap classes always
works by-reference rather than by-value. This is caused by the fact that one can not define
custom `equals` and `hashCode` implementations for value classes.

### @enum class

`@enum` classes lets one define closed tagged unions of other offheap classes.

```scala
  @enum class Response
  object Response {
    @data class Success()
    @data class Failure(error: Long)
  }
```

Once another class is nested within an `@enum` it automatically gets an extra hidden tag
field that lets one perform: pattern matching, instance checks (through `is[T]`) and
memory-safe casts (through `as[T]`).

Relation between `@enum` class and it's children looks and feel very much like they can
are actual proper subclasses:

```scala
  val response: Response = Response.Failure(404)
  response match {
    case Response.Success()     => println("succeeded")
    case Response.Failure(code) => println(s"failed with code $code")
  }
```

Although this is not in fact correct as all ofheap classes desugar into value classes
which do not support any inheritence other than inheritance from universal traits.
Above code works because `@enum` supplies implicit coercions that let
one automatically upcast up the `@enum` chain.

It's also possible to nest one `@enum` into the other.

```scala
@enum class Tree
object Tree {
  @enum object Term { ... }
  @enum object Type { ... }
}
```

Nested enums can use shorthand form that annotation object rather than the class. This
shorthand form can be used whenever an `@enum` class is defined inside other objects and
not on the top-level (caused by restrictions of macro-annotations.)

## How to contribute

1. Check the list of [open issues](https://github.com/densh/scala-offheap/issues) and see
   if you are interested in fixing any of them. If you have encountered a problem or have
   feature suggestion feel free opening up a new issue.
1. Fork the [main repo](https://github.com/densh/scala-offheap) and start hacking up
   the fix. If you have problems with getting started contact
   [@densh](https://github.com/densh) to help you out.
1. Whenever you fix an issue, add a test that shows that it was indeed fixed. If you
   introduce a new feature, add a new test suite with a bunch of tests that cover common
   use cases. If you propose a performance enhancement include before & after results of
   corresponding jmh performance benchmark run in the commit message.
1. Fire up a pull request. Don't forget to sign
   [Scala CLA](http://typesafe.com/contribute/cla/scala) and add yourself to the
   [list of contributors](https://github.com/densh/scala-offheap/blob/master/AUTHORS.md).
