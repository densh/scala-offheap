# Safety

## Memory safety

There are two common classes of memory safety:

1. **Spatial memory safety** that guarantees that one can not get out of bounds of
   the claimed memory blocks.
2. **Temporal memory safety** that ensures that it's impossible to dereference memory
   that has already been freed.

scala-offheap's typed off-heap classes and bounds-checked arrays provide the first
guarantee as long as you stay in typed world and don't use lower level parts of
the api: `malloc`, `addr/fromAddr`, `Memory`, `asInstanceOf` etc. It's possible to
disable bounds checks using `-Doffheap.unchecked.bounds` runtime flag.

At the moment we do not guarantee temporal safety by default.

It's possible to enable checked memory mode that provides temporal safety for region
allocations at expense of 1.6-2x slowdown of memory accesses and 65k limit on
the number of open regions at any given time.
Under this mode dereferencing of pointers to regions that has already been closed
throws `offheap.InaccessibleMemoryException`.

One can enable memory checked mode by passing `-Doffheap.checked.memory` to the JVM
that runs your scala-offheap application. The setting is global and applies to all
memory accesses in the application. This mode is only intended for
development and debugging and is not recommended for production use.

## Null safety

scala-offheap does provide basic dynamic checks that ensure that null pointers
are never dereferenced and appropriate `NullPointerException` is thrown.

It's possible to disable this checks by passing `-Doffheap.unchecked.null` flag to
the JVM that runs your application for minor (often insignificant) performance gain.
Null dereference with the check disabled is undefined behaviour. The setting is global.

## Thread safety

All of the standard allocators in scala-offheap are thread-safe and can be safely
shared between multiple running threads.

Allocated objects on the other hand do not provide any guarantees in situations
when they are mutated from multiple threads. Therefore it's highly recommended to
never share mutable off-heap allocations between multiple threads.
