package offheap.test.jmh

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import offheap._

@State(Scope.Thread)
class OffheapAllocation {
  @Param(Array("linked", "slinked", "caslinked", "stack", "sstack"))
  var allocator: String = _

  var r: Region = _

  @Setup(Level.Iteration)
  def setup(): Unit =
    r = allocator match {
      case "linked"    => new internal.LinkedRegion
      case "slinked"   => new internal.SynchronizedLinkedRegion
      case "caslinked" => new internal.CASLinkedRegion
      case "stack"     => new internal.AddrStackRegion
      case "sstack"    => new internal.SynchronizedAddrStackRegion
    }

  @TearDown(Level.Iteration)
  def tearDown(): Unit =
    r.close()

  @Benchmark
  def offheapPoint1() =
    OffheapPoint1(10)(r)

  @Benchmark
  def offheapPoint2() =
    OffheapPoint2(10, 20)(r)

  @Benchmark
  def offheapPoint4() =
    OffheapPoint4(10, 20, 30, 40)(r)
}

class GCAllocation {
  @Benchmark
  def point1(): Point1 =
    new Point1(10)

  @Benchmark
  def point2() =
    new Point2(10, 20)

  @Benchmark
  def point4() =
    new Point4(10, 20, 30, 40)
}
