package jmh

import org.openjdk.jmh.annotations._
import offheap._, internal.SunMisc.UNSAFE

@State(Scope.Thread)
class LoopBench {
  var length = 10000
  val uarr: Long = {
    val addr = UNSAFE.allocateMemory(length * 4)
    var i = 0
    while (i <= length) {
      UNSAFE.putInt(addr + i * 4, i)
      i += 1
    }
    addr
  }

  @Benchmark
  def unsafeSum1 = {
    var sum = 0
    val bound = length
    var i = 0
    while (i != bound) {
      sum += UNSAFE.getInt(uarr + 4 * i)
      i += 1
    }
    sum
  }

  @Benchmark
  def unsafeSum2 = {
    var sum = 0
    val bound = uarr + 4 * length
    var p = uarr
    while (p != bound) {
      sum += UNSAFE.getInt(p)
      p += 4
    }
    sum
  }
}
