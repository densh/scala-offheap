package jmh

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import offheap._

class JAlloc8(val a: Long)
class JAlloc16(val a: Long, val b: Long)
class JAlloc32(val a: Long, val b: Long, val c: Long, val d: Long)
class JAlloc64(val a: Long, val b: Long, val c: Long, val d: Long,
               val e: Long, val f: Long, val g: Long, val h: Long)

@State(Scope.Thread)
class OffheapAllocation {
  val pool = Pool(malloc)
  @Param(scala.Array("malloc"))
  var impl: String = _
  @Param(scala.Array("16", "64", "256", "1024", "4096"))
  var n: Int = _

  var alloc: Allocator = _
  var addr: Addr = _
  var disposeAlloc: () => Unit = _
  var disposeAddr: Addr => Unit = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    val (a, d1, d2) = impl match {
      case "malloc"        =>
        (malloc, () => (), malloc.free(_))
      case "pool-region"   =>
        val r = PoolRegion.open(pool)
        (r, () => r.close, (addr: Addr) => ())
      case "direct-region" =>
        val r = DirectRegion.open(malloc)
        (r, () => r.close, (addr: Addr) => ())
    }
    alloc = a
    disposeAlloc = d1
    disposeAddr = d2
  }

  @TearDown(Level.Iteration)
  def tearDownIteration(): Unit = disposeAlloc()

  @TearDown(Level.Invocation)
  def tearDownInvocation(): Unit = disposeAddr(addr)

  @Benchmark
  def allocate =
    // compensate for 8 byte header to get n-byte allocation
    addr = Array.uninit[Byte](n - 8)(alloc).addr
}

@State(Scope.Thread)
class GCAllocation {
  @Param(scala.Array("16", "64", "256", "1024", "4096"))
  var n: Int = _

  @Benchmark
  def allocate =
    // compensate for 12 byte header to get n-byte allocation
    new scala.Array[Byte](n - 12)
}
