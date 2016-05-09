# Off-heap arrays

## Array[T]

A direct off-heap equivalent of regular `scala.Array`. A subset of common operations
provided by `ArrayOps` is implemented in macro-optimised way to eliminate closure
allocation and boxing overhead through inline their bodies into the iteration loop.

**Data layout.** Array layout contains of 8-byte header that stores length of the array
followed by values of the array type in the order of their indexes. Offheap classes are
stored as `Addr` references.

**Methods.**

* `arr(index)`. Get an element with given index.
* `arr(index) = value`. Set an element with given index to given value.
* `arr.isEmpty`, `arr.nonEmpty`. Check if given array is empty.
* `arr.size`, `arr.length`. Get a size of given array.
* `arr.foreach(f)`. Call function on every element and discard the result.
* `arr.map(f)`. Call function on every element and create a new array with its results.
* `arr.transform(f)`. Call function on every element and store its result back at the same position.
* `arr.toArray`. Create an on-heap copy of given array.
* `arr.clone`. Create and off-heap copy of given array.
* `arr.forall(f)`. Check if given predicate applies to all elements in the array.
* `arr.exists(f)`. Check if given predicate applies to at least one element in the array.
* `arr.filter(f)`. Create a new array where each element satifies given predicate.
* `arr.foldLeft(z)(op)`, `arr.foldRight(z)(op)`. Applies given operator to a starting value and all elements of 
  this array going from left or right.
* `arr.reduceLeft(op)`, `arr.reduceRight(op)`. Applies given operator to all elements of this array going from
  left or right.
* `arr.sameElements(other)`. Check if this and given array have the same elements in the same order.
* `arr.startsWith(that)`. Check if this array starts with the same elements as the given array.
* `arr.startsWith(that, offset)`. Check if this array starts with the same elements as the given array, 
  starting at given offset.
* `arr.endsWith(that)`. Check if this array ends with the same elements as the given array.

**Companion methods.**

* `Array(value1, ..., valueN)`. Create an array of size `N` with given values.
* `Array.uninit[T](size)`. Create an uninitilized array of given size.
* `Array.fill(size) { value }`. Create an array of given size initialized to `value`.
* `Array.fromArray(sarr)`. Create an off-heap copy of given on-heap array `sarr`.
* `Array.empty[T]`. Get an empty array of given type `T`.
* `Array.fromAddr[T](addr)`. Reinterpret given address as array of given type `T`.
* `Array.copy(arr1, index1, arr2, index2, size)`. Copy subrange of one array into the other.

**Limitations.**

* Can not contain values that refers to on-heap objects.
* Can not be used in typical generic contexts. Array element type must
  be known statically.
* Not all of the `ArrayOps` functionality is implemented. If you are missing
  some particular method file a feature request on our issue tracker.
* Arrays are `Int`-sized. In our testing we discovered that this improves performance
  over `Long`-sized arrays. In the future we might introduce an alternative big arrays
  that do not make this trade-off.

## EmbedArray[T]

An alternative implementation of arrays that includes off-heap classes as values rather
than as references to reduce dereferencing overhead and improve data locality.
Direct equivalent of dynamically sized array of structs in C.

**Methods.** Same as for regular arrays, except for the methods which rely on equality of
elements like `sameElements`.

**Companion methods.** Same as for regular arrays.

**Limitations.**

* Can only store values of off-heap classes.
* Can not be used in generic contexts.
* Only a subset of `ArrayOps` functionality.
* `Int`-sized.
