# Allocators

Allocators come as implementations of the `Allocator` trait:

```scala
trait Allocator {
  def allocate(size: Size): Addr
  def reallocate(addr: Addr, size: Size): Addr
  def free(addr: Addr): Unit
}
```

This trait defines low-level details of how memory allocations are performed. Whenever
an off-heap class or array is allocated it's going to claim memory using `allocate`
method on the allocator it has been given.

De-allocation strategy depends on the allocator that is being used.
Managed allocators like regions might automatically perform resource clean up whenever
some condition is met (e.g. region is closed.) It's also typical for managed allocators
to not support `reallocate` and `free` as those are performed automatically.
Unmanaged allocators like `malloc` and `jemalloc` require user to manually
call `free` whenever the allocated object is not needed any longer.

## Unmanaged allocators

Canonical unmanaged allocator is system-provided `malloc`.

```
scala> implicit val alloc: Allocator = malloc
alloc: offheap.Allocator = offheap.malloc$@23d230db

scala> val arr = Array(1, 2, 3)
arr: offheap.Array[Int] = offheap.Array@a0978d6a

scala> arr.foreach(println)
1
2
3

scala> alloc.free(arr.addr)
```

All unmanaged allocators require one to explicitly call `free`. If it's not called the
applications is going to leak memory. It's highly recommended to only use such
allocators for data that has to be permanently available throughout application
lifetime. Regions provide much safer interface for temporary allocations.

It is also important to remember that data collections returned by methods such as
`map` or `filter` also need to be freed. In particular one shouldn't chain calls to those
methods like `.filter(...).map(...)` since the intermediary result is guaranteed to cause
a memory leak.

## Region allocators

Region allocators are a family of semi-automatic memory allocators that are based upon
the idea of tying resource management to some particular scope in the program. They have
been largely inspired by previous work done in
[Cyclone programming language](http://dl.acm.org/citation.cfm?id=512563).

For example:

```scala
scala> implicit val props = Region.Props(Pool())
props: offheap.PoolRegion.Props = Props(offheap.Pool@5773b847)

scala> Region { implicit r =>
     |   val arr = Array(1, 2, 3)
     |   arr.foreach(println)
     | }
1
2
3
```

Here we allocate an array of three elements in region `r`. As long as execution stays
within region block the array will stay in memory and will be available. Once the
execution leaves the block all of the allocations performed in region are going
to be cleaned up at once.

Unlike unmanaged allocators regions do not require one to micro-manage allocations on
allocation-by-allocation basis. Moreover standard scoped syntax ensures that resources
are going to be eventually cleaned up. Apart from minimized syntactical and mental
overhead regions can also significantly improve allocation performance over regular
unmanaged allocators.

General concept of region is not tied to the specific memory management strategy.
`Region.Props` provide configuration necessary to instantiate specific implementation.
At the moment we provide a single implementation based on page memory pooling.

Apart from scoped syntax one can also call `Region.open` and `close` manually to
integrate them into situations with undetermined lifetimes:

```scala
scala> implicit val props = Region.Props(Pool())
props: offheap.PoolRegion.Props = Props(offheap.Pool@7f71be69)

scala> implicit val r = Region.open
r: offheap.Region = offheap.PoolRegion@70bb5afe

scala> ...

scala> r.close
```

This is helpful if one wants to close region at GC-collected object finalization time.
Not closing region is going to leak memory so it's generally recommended to use scoped
syntax whenever possible.

## Memory-pool-based regions

Main goal of memory-pool-based regions is to deliver consistently good allocation
performance that doesn't depend on quality of underlying system allocator.

This is achieved by allocating reasonably large chunks of memory (1M by default) and
re-using that memory throughout the lifetime of memory pool. In this scheme regions
borrow memory from memory pool in fixed-size pages (4K by default) and return them
back once they are closed.

In the end we get significantly improved performance over allocations from system
allocator (up to 3x faster) at expense of a few limitations:

1. One can not allocate objects larger than a memory pool page.
2. Memory allocated by the pool is not reclaimed back to underlying allocator until
   its finalizer is called.

Pool-based region management is default implementation strategy that you use when you
instantiate `Region.Props` through default factory method that takes a memory pool
instance as a parameter:

```scala
scala> val explicit = Region.Props(Pool())
props: offheap.PoolRegion.Props = Props(offheap.Pool@136afba1)
```

Here `default` and `explicit` both instantiate a new memory pool with default
configuration. Pools can also be given following (optional) arguments that let
one tune pooling for their application needs:

1. `alloc: Allocator` that is set to `malloc` by default.
2. `pageSize: Size` that defaults to undelying OS virtual page size (which is
    usually 4K.)
3. `chunkSize: Size` that defaults to the size of 256 virtual pages (which is
    usually 1M.)
