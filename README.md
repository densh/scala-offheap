# Type-safe off-heap memory for Scala

[![Build Status](https://travis-ci.org/densh/scala-offheap.svg)](https://travis-ci.org/densh/scala-offheap)
[![Join the chat at https://gitter.im/densh/scala-offheap](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/densh/scala-offheap)


Garbage collection is the standard memory management paradigm on the JVM. In theory, it lets one
completely forget about the hurdles of memory management and delegate all of it to the underlying
runtime. In practice, GC often leads to scalability issues on large heaps and latency-sensitive
workloads.

The goal of this project is to expose a completely different memory management
paradigm to the developers: explicitly annotated region-based memory. This paradigm gives
more control over memory management without the need to micro-manage allocations.

```scala
@data class Dummy(id: Int) {
  def hello: Unit = println(s"Hello, i'm $id")
}

Region { implicit r =>
  for (i <- 1 to 100)
     Dummy(i).hello
}
```

For example the snippet above allocates 100 objects in a memory region.
As long as the region is open, objects are retained in memory and available for access.
Once it ends, all of them are efficiently deallocated at once.

## Features

* Efficient scoped region-based memory allocator
* Optional low-overhead memory sanitizer for debugging and development
* Offheap classes as a nice typed API for custom data layout
* Offheap arrays with direct sequential layout in memory
* Extensibility to accomodate custom memory allocators

## Documentation

Documentation is available at the project's
[wiki page](https://github.com/densh/scala-offheap/wiki).

## How to contribute

1. Check the list of [open issues](https://github.com/densh/scala-offheap/issues) and see
   if you are interested in fixing any of them. If you have encountered a problem or have
   a feature suggestion feel free to open a new issue.
1. Fork the [main repo](https://github.com/densh/scala-offheap) and start hacking up
   the fix. If you have problems with getting started, contact
   [@densh](https://github.com/densh) to help you out.
1. Whenever you fix an issue, add a test that shows that it was indeed fixed. If you
   introduce a new feature, add a new test suite with a bunch of tests that cover common
   use cases. If you propose a performance enhancement, include before & after results of
   a corresponding jmh performance benchmark run in the commit message.
1. Fire up a pull request. Don't forget to sign the
   [Scala CLA](http://typesafe.com/contribute/cla/scala).
