package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap.x64._

@State(Scope.Thread)
class Access {
  implicit val pool: Pool = Pool(UnsafeMemory)
  var r: Region = _
  var op1: OffheapPoint1 = _
  var op2: OffheapPoint2 = _
  var op4: OffheapPoint4 = _
  var p1: Point1 = _
  var p2: Point2 = _
  var p4: Point4 = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    r = Region.open
    op1 = OffheapPoint1(10)(r)
    op2 = OffheapPoint2(10, 20)(r)
    op4 = OffheapPoint4(10, 20, 30, 40)(r)
    p1 = new Point1(10)
    p2 = new Point2(10, 20)
    p4 = new Point4(10, 20, 30, 40)
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = r.close

  @Benchmark
  def offheapPoint1Field1()= op1.a

  @Benchmark
  def offheapPoint2Field1()= op2.a

  @Benchmark
  def offheapPoint2Field2()= op2.b

  @Benchmark
  def offheapPoint4Field1()= op4.a

  @Benchmark
  def offheapPoint4Field2()= op4.b

  @Benchmark
  def offheapPoint4Field3()= op4.c

  @Benchmark
  def offheapPoint4Field4()= op4.d

  @Benchmark
  def point1Field1() = p1.a

  @Benchmark
  def point2Field1() = p2.a

  @Benchmark
  def point2Field2() = p2.b

  @Benchmark
  def point4Field1() = p4.a

  @Benchmark
  def point4Field2() = p4.b

  @Benchmark
  def point4Field3() = p4.c

  @Benchmark
  def point4Field4() = p4.d
}
