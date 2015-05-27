package offheap.test.jmh

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import offheap._

class Point(val x: Int, val y: Int)

@data class OffheapPoint(val x: Int, val y: Int)

@State(Scope.Thread)
class OffheapAllocation {
  implicit val pool: Pool = Pool(SystemAllocator)
  var r: Region = _

  @Setup(Level.Iteration)
  def setup(): Unit = r = Region.open

  @TearDown(Level.Iteration)
  def tearDown(): Unit =
    r.close()

  @Benchmark
  def offheapPoint() =
    OffheapPoint(10, 20)(r)
}

class GCAllocation {
  @Benchmark
  def point() =
    new Point(10, 20)
}
