package jmh

import org.openjdk.jmh.annotations._
import offheap._

@data class AlignedInt0(v: Int, b1: Byte, b2: Byte, b3: Byte, b4: Byte)
@data class AlignedInt1(b1: Byte, v: Int, b2: Byte, b3: Byte, b4: Byte)
@data class AlignedInt2(b1: Byte, b2: Byte, v: Int, b3: Byte, b4: Byte)
@data class AlignedInt3(b1: Byte, b2: Byte, b3: Byte, v: Int, b4: Byte)

@data class AlignedLong0(v: Long,  b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte)
@data class AlignedLong1(b1: Byte, v: Long,  b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte)
@data class AlignedLong2(b1: Byte, b2: Byte, v: Long,  b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte)
@data class AlignedLong3(b1: Byte, b2: Byte, b3: Byte, v: Long,  b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte)
@data class AlignedLong4(b1: Byte, b2: Byte, b3: Byte, b4: Byte, v: Long,  b5: Byte, b6: Byte, b7: Byte, b8: Byte)
@data class AlignedLong5(b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, v: Long,  b6: Byte, b7: Byte, b8: Byte)
@data class AlignedLong6(b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, v: Long,  b7: Byte, b8: Byte)
@data class AlignedLong7(b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, v: Long,  b8: Byte)

@State(Scope.Thread)
class FieldAlignment {
  implicit val props = Region.Props()

  var rl: Region = _
  var ri: Region = _

  var i0: AlignedInt0 = _
  var i1: AlignedInt1 = _
  var i2: AlignedInt2 = _
  var i3: AlignedInt3 = _

  var l0: AlignedLong0 = _
  var l1: AlignedLong1 = _
  var l2: AlignedLong2 = _
  var l3: AlignedLong3 = _
  var l4: AlignedLong4 = _
  var l5: AlignedLong5 = _
  var l6: AlignedLong6 = _
  var l7: AlignedLong7 = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    rl = Region.open
    ri = Region.open

    l0 = AlignedLong0(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l1 = AlignedLong1(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l2 = AlignedLong2(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l3 = AlignedLong3(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l4 = AlignedLong4(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l5 = AlignedLong5(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l6 = AlignedLong6(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)
    l7 = AlignedLong7(0, 0, 0, 0, 0, 0, 0, 0, 0)(rl)

    i0 = AlignedInt0(0, 0, 0, 0, 0)(ri)
    i1 = AlignedInt1(0, 0, 0, 0, 0)(ri)
    i2 = AlignedInt2(0, 0, 0, 0, 0)(ri)
    i3 = AlignedInt3(0, 0, 0, 0, 0)(ri)
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    rl.close
    ri.close
  }

  @Benchmark def alignedInt0() = i0.v
  @Benchmark def alignedInt1() = i1.v
  @Benchmark def alignedInt2() = i2.v
  @Benchmark def alignedInt3() = i3.v

  @Benchmark def alignedLong0() = l0.v
  @Benchmark def alignedLong1() = l1.v
  @Benchmark def alignedLong2() = l2.v
  @Benchmark def alignedLong3() = l3.v
  @Benchmark def alignedLong4() = l4.v
  @Benchmark def alignedLong5() = l5.v
  @Benchmark def alignedLong6() = l6.v
  @Benchmark def alignedLong7() = l7.v
}
