# Scala Offheap: Managing Offheap Memory through first-class objects

## Memory and Ref

## Offheap classes

Offheap classes come in two flavors: `@enum` and `@data`. Both of them desugar down to a
value classes over offheap references. This lets them both enjoy low-overhead nature of
the value classes but at the same time share some of their deficiencies.

### @data

`@data` classes are aimed to be an offheap equivalent of case classes. Apart from field
accessors (and possibly setters) they also generate automatic name-based pattern matching
support methods (`isEmpty`, `nonEmpty`, `get`, `_1`, ... , `_N`), `copy` method,
`toString` method and memory-safe casting primitives `is[T]` and `as[T]`.

```scala
  @data class Point(x: Int, y: Int)
```

To allocate a data object one needs to have an implicit memory in scope.

```scala
  implicit val memory = UnsafeMemory()
  val point = Point(10, 20)
```

Similarly to case classes one can have methods and type alias definitions inside them.
Due to restrictions of values classes, traits or objects in the body of @data class.

Another important distinction is the fact that comparison on offheap classes always
works by-reference rather than by-value. This is caused by the fact that one can not define
custom `equals` and `hashCode` implementations for value classes.

### @enum

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

## Offheap arrays

## Regions and memory pools

## Memory safety

## How to contribute
