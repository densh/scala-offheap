package jmh

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import java.util.concurrent.TimeUnit
import offheap._, internal.SunMisc.UNSAFE

@State(Scope.Thread)
class Array {
  implicit val alloc = malloc
  implicit val props = Region.Props(Pool(malloc, pageSize = 81920, chunkSize = 81920))
  val jarr: scala.Array[Long] = (0 to 9999).toArray.map(_.toLong)
  val arr: offheap.Array[Long] = {
    val arr = offheap.Array.uninit[Long](10000)
    for (i <- 0 to 9999) arr(i) = i
    arr
  }
  val uarr: Long = {
    val addr = UNSAFE.allocateMemory(10000 * 8)
    var i = 0
    while (i <= 10000) {
      UNSAFE.putLong(addr + i * 8, i)
      i += 1
    }
    addr
  }

  val _0 = 0
  val _42 = 42

  @Benchmark
  def offheapAccess = arr(_0)

  @Benchmark
  def onheapAccess = jarr(_0)

  @Benchmark
  def unsafeAccess = UNSAFE.getLong(uarr + _0)

  @Benchmark
  def offheapUpdate = { arr(_0) = _42; _42 }

  @Benchmark
  def onheapUpdate = { jarr(_0) = _42; _42 }

  @Benchmark
  def unsafeUpdate = { UNSAFE.putLong(uarr + _0, _42); _42 }

  @Benchmark
  def offheapSum = {
    val len = arr.length
    var sum = 0L
    var i = 0
    while (i < len) {
      sum += arr(i)
      i += 1
    }
    sum
  }

  @Benchmark
  def unsafeSum = {
    val len = arr.length
    var sum = 0
    var i = 0
    while (i < len) {
      sum += UNSAFE.getInt(uarr + 4 * i)
      i += 1
    }
    sum
  }

  @Benchmark
  def onheapSum = {
    val len = jarr.length
    var sum = 0L
    var i = 0
    while (i < len) {
      sum += jarr(i)
      i += 1
    }
    sum
  }

  @Benchmark
  def offheapForeach(bh: Blackhole) = arr.foreach { v => bh.consume(v) }

  @Benchmark
  def onheapForeach(bh: Blackhole) = jarr.foreach { v => bh.consume(v) }

  @Benchmark
  def offheapMap = Region { r => arr.map(_ * 2)(r) }

  @Benchmark
  def onheapMap = jarr.map(_ * 2)

}
