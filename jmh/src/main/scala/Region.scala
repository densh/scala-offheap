package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class RegionClose {
  var r: Region = _

  @Param(Array("linked", "slinked", "caslinked", "stack", "sstack"))
  var allocator: String = _

  @Param(Array("1", "2", "4", "8", "16", "32", "64", "128", "256", "512", "1024"))
  var allocatedPages: Int = _

  @Setup(Level.Invocation)
  def setup(): Unit = {
    r = allocator match {
      case "linked"    => new internal.LinkedRegion
      case "slinked"   => new internal.SynchronizedLinkedRegion
      case "caslinked" => new internal.CASLinkedRegion
      case "stack"     => new internal.AddrStackRegion
      case "sstack"    => new internal.SynchronizedAddrStackRegion
    }
    for (_ <- 1 to allocatedPages)
      internal.Region.allocate(r, internal.PAGE_SIZE)
  }

  @Benchmark
  def close = r.close()
}

@State(Scope.Thread)
class RegionOpen {
  var r: Region = _

  @TearDown(Level.Invocation)
  def tearDown(): Unit = internal.Region.close(r)

  @Benchmark
  def open = {
    r = internal.Region.open()
    r
  }
}

@State(Scope.Thread)
class RegionAllocateCurrent {
  var r: Region = _

  @Param(Array("linked", "slinked", "caslinked", "stack", "sstack"))
  var allocator: String = _

  @Setup(Level.Invocation)
  def setup(): Unit =
    r = allocator match {
      case "linked"    => new internal.LinkedRegion
      case "slinked"   => new internal.SynchronizedLinkedRegion
      case "caslinked" => new internal.CASLinkedRegion
      case "stack"     => new internal.AddrStackRegion
      case "sstack"    => new internal.SynchronizedAddrStackRegion
    }

  @TearDown(Level.Invocation)
  def tearDown(): Unit = r.close()

  @Benchmark
  def allocate = r.allocate(16)
}

@State(Scope.Thread)
class RegionAllocateNext {
  var r: Region = _

  @Param(Array("linked", "slinked", "caslinked", "stack", "sstack"))
  var allocator: String = _

  @Setup(Level.Invocation)
  def setup(): Unit = {
    allocator match {
      case "linked"    => new internal.LinkedRegion
      case "slinked"   => new internal.SynchronizedLinkedRegion
      case "caslinked" => new internal.CASLinkedRegion
      case "stack"     => new internal.AddrStackRegion
      case "sstack"    => new internal.SynchronizedAddrStackRegion
    }
    internal.Region.allocate(r, internal.PAGE_SIZE)
  }

  @TearDown(Level.Invocation)
  def tearDown(): Unit = r.close()

  @Benchmark
  def allocate = r.allocate(16)
}
