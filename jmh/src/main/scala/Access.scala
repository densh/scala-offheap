package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class Access {
  var r: Region = _
  var op1: OffheapPoint1 = _
  var op2: OffheapPoint2 = _
  var op4: OffheapPoint4 = _
  var p1: Point1 = _
  var p2: Point2 = _
  var p4: Point4 = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    r = internal.Region.open()
    op1 = OffheapPoint1(10)(r)
    op2 = OffheapPoint2(10, 20)(r)
    op4 = OffheapPoint4(10, 20, 30, 40)(r)
    p1 = new Point1(10)
    p2 = new Point2(10, 20)
    p4 = new Point4(10, 20, 30, 40)
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit =
    internal.Region.close(r)

  @Benchmark
  def offheapPoint1Field1()= op1._1

  @Benchmark
  def offheapPoint2Field1()= op2._1

  @Benchmark
  def offheapPoint2Field2()= op2._2

  @Benchmark
  def offheapPoint4Field1()= op4._1

  @Benchmark
  def offheapPoint4Field2()= op4._2

  @Benchmark
  def offheapPoint4Field3()= op4._3

  @Benchmark
  def offheapPoint4Field4()= op4._4

  @Benchmark
  def point1Field1() = p1._1

  @Benchmark
  def point2Field1() = p2._1

  @Benchmark
  def point2Field2() = p2._2

  @Benchmark
  def point4Field1() = p4._1

  @Benchmark
  def point4Field2() = p4._2

  @Benchmark
  def point4Field3() = p4._3

  @Benchmark
  def point4Field4() = p4._4
}
