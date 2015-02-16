package offheap.test.jmh

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import offheap._

@State(Scope.Thread)
class Allocation {
  var r: Region = _

  @Setup
  def setup(): Unit =
    r = internal.Region.open()

  @TearDown
  def tearDown(): Unit =
    internal.Region.close(r)

  @Benchmark
  def offheapPoint1() =
    OffheapPoint1(10)(r)

  @Benchmark
  def offheapPoint2() =
    OffheapPoint2(10, 20)(r)

  @Benchmark
  def offheapPoint4() =
    OffheapPoint4(10, 20, 30, 40)(r)

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
