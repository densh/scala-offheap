# Off-heap classes

Off-heap classes are core abstraction that lets one define custom data structures to
be allocated in off-heap memory. Unlike regular classes they are completely opaque
to JVM's garbage collector (which sees them just as values of `Long` primitive type)
and incur no typical GC overhead that regular classes do.

To allocate anything off heap one has to provide an instance of
`Allocator` type that defines memory management strategy for current allocation.
[[Allocators]] are going to be covered in detail in the next section
and for now we just assume that there is one available in the scope. E.g.:

```scala
scala> implicit val alloc: Allocator = malloc
alloc: offheap.Allocator = offheap.malloc$@23d230db
```

Off-heap classes come in two varieties: `@data` classes and `@enum` classes. Both of
them desugar down to value classes over underlying physical address. Due to that they:

* can only be defined in statically accessible locations
* may not define custom equals or hashCode
* can only extend universal traits
* have some limitation on nested members

This is a price one has to pay for efficient unboxed representation.

## @data classes

Data classes are a close equivalent of on-heap case classes. For example:

```scala
@data class Point(x: Int, y: Int)
```

Defines a data class with two immutable publicly accessible fields `x` and `y`. Data
classes are usually passed by reference just regular normal classes. For example if
we define another data class that contains two fields of `Point` type:

```scala
@data class Segment(start: Point, end: Point)
```

We would store two references to two points allocated somewhere. It's possible to
change standard by-reference semantics into by-value one using `@embed` annotion:

```scala
@data class Segment2(@embed start: Point, @embed end: Point)
```

Here `start` and `end` are not references to `Point` but rather actual values within
`Segment2` allocation. Whenever an embedded field is accessed an inner pointer for given
field is computed and returned as a value of given type without any extra dereferencing
overhead.

Whenever and off-heap class with embed fields is instantiated using canonical constructors
the construction of embedded fields happens right within outer classes memory. For example
in the following snippet:

```scala
scala> Segment2(Point(10, 20), Point(30, 40))
res0: Segment2 = Segment2(Point(10, 20), Point(30, 40))
```

Values of `x` and `y` for `start` and `end` points are written directly into memory
allocated for the segment. If we were to split this code apart and allocate points
outside of the `Segment2` constructor:

```scala
scala> val start = Point(10, 20)
start: Point = Point(10, 20)

scala> val end = Point(30, 40)
end: Point = Point(30, 40)

scala> Segment2(start, end)
res1: Segment2 = Segment2(Point(10, 20), Point(30, 40))
```

The end result would be the same but the point values would first be allocated separately
using current allocator in scope and then copied over to segment memory. Needless to say
this is going to be less efficient and will allocate more memory that has to be taken care
off in one way or the other.

At the moment one can only embed other off-heap classes. Primitives have embed-like
semantics by default. Arrays can not be embedded due to their variable size nature. We
plan to add support for
[embeddable fixed-size arrays](https://github.com/densh/scala-offheap/issues/47) in the future.

Embed annotation only takes effect when it's used on fields of the data class and does
nothing if put into any other location (e.g. on local val definition.)

**Data layout.** Fields of data class are laid out in memory in the same order
as they were declared. Appropriate padding is inserted between the fields so that
each field is aligned according to the size of its type. For example:

```scala
cala> @data class XYZ(x: Byte, y: Char, z: Long)
defined class XYZ
defined object XYZ
```

Is going to take 16 bytes, not 9. 1-byte padding is going to be inserted after `x` and
4-byte padding is going to be inserted after `y`. Reordering the fields can sometimes
reduce the size of the data structure.

One can inspect layout of the data class using `sizeOfEmbed` and `offsetOf` helper
functions:

```scala
scala> sizeOfEmbed[XYZ]
res1: offheap.Size = 16

scala> offsetOf[XYZ]("x")
res2: offheap.Size = 0

scala> offsetOf[XYZ]("y")
res3: offheap.Size = 2

scala> offsetOf[XYZ]("z")
res4: offheap.Size = 8
```

Embedded fields of type `T` are treated as a large primitive value with a `sizeOfEmbed[T]`
size and aligned to the size of their largest field that one can look up using
`alignmentOfEmbed[T]` utility function.

**Automatically generated members.** Apart from field accessors such
classes get following automatically generated methods:

1. `isEmpty`, `nonEmpty`. `isEmpty` returns true whenever current point value is
   a null reference.
1. `get`, `_1`, ..., `_N`. These are necessary for name-based pattern
   matching support that lets us avoid intermediate on-heap allocations like
   tuples and options.
1. `copy`. Similarly to `copy` on case classes it lets one create a new instance of
   a class by overriding values of some fields. Also takes an implicit allocator that
   is going to be used to allocate new instance.
1. `as`. An equivalent of `asInstanceOf` within off-heap hierarchy.
1. `is`. An equivalent of `isInstanceOf` within off-heap hierarchy.
1. `addr`. Underlying physical address that corresponds to this instance.
1. `toString`. With sane automatic implementation.
1. `equals` and `hashCode` that are based on the underlying physical address.

One also gets following methods generated into companion of the class:

1. `apply`. Takes values for all fields and an implicit allocator.
1. `unapply`. Adds support for destructuring of off-heap classes.
1. `empty`. Canonical null reference of data class type. Off-heap classes can not be
    assigned actual `null` literal due to the fact that they are represented as value
    classes.
1. `fromAddr`. Reinterpret given physical address as an instance of given class.

**Limitations.** Even though data classes share declaration syntax with regular classes
they have a few limitations that are often caused by limitations of values classes
and macros that are used to implement them:

* Data classes may only contain following statements in their body:
  1. Non-abstract methods.
  1. Non-abstract type aliases.
  1. Extra field declarations with explicitly annotated types.
  1. Initialization statements.
* Data classes may only inherit from universal traits.
* Data classes may not define their own implementations of pre-generated methods.
* Data classes may not have type parameters.
* Data classes may not have early initializers.
* Data classes may not have more than 64 constructor arguments.
* Data classes may not have more than one constructor argument list.
* Data classes may not have implicit arguments.
* Data classes may not have secondary constructors.
* Data classes may not have fields that refer to on-heap classes
  (i.e. all fields should either be of primitive or off-heap object types.)

## @enum classes

Enum classes are an off-heap equivalent of sealed abstract class hierarchy. For example:

```scala
@enum class Figure
object Figure {
  @data class Point(x: Float, y: Float)
  @data class Segment(from: Point, to: Point)
  @data class Circle(center: Point, radius: Float)
}
```

It's also possible to nest enums.

```scala
@enum class Figure
object Figure {
  @enum class _2D
  object _2D {
    @data class Point(x: Float, y: Float)
    ...
  }
  @enum class _3D
  object _3D {
    @data class Point(x: Float, y: Float, z: Float)
    ...
  }
}
```


Whenever inner enum doesn't define any methods it's possible to use shorthand syntax
that annotates nested object as `@enum` without a need to explicitly define accompanying
class (it will be generated automatically):

```scala
@enum class Figure
object Figure {
  @enum object _2D { ... }
  @enum object _3D { ... }
}
```

**Data layout.** Enum classes are represented as a tagged unions of the nested off-heap
classes. Layout-wise this has an effect of having one extra hidden tag field (of the smallest
numeric type appropriate to enumerate all nested classes) to each and every children
data class.

This field is used to implement pattern matching and `is` and `as` helpers. In situations
when enum classes are nested one into the other, the hidden tag field is introduced only
once per top-level enum class. Nested enum classes share the same field.

**Automatically generated members.** Enum classes generates a subset of methods
we've seen before with the same semantics as in data classes:

1. `isEmpty`, `nonEmpty`.
1. `as`.
1. `is`.
1. `addr`.
1. `toString`.
1. `equals` and `hashCode`.

And corresponding companion members:

1. `empty`.
1. `fromAddr`.
1. An implicit conversion from child class types to parent class types that corresponds
   to upcasting semantics for regular classes. For example code like:

       val f: Figure = Figure.Point(10, 20)

   Is supported thanks to this conversion.

**Limitations.**

* Enum classes may only contain non-abstract methods in their body.
* Enum classes may only inherit from universal traits.
* Enum classes may not have any constructors
* Enum classes may not define their own implementations of pre-generated methods.
* Enum classes may not have type parameters.
* Enum classes may not have early initializers.
