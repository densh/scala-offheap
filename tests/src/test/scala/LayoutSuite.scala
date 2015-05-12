package test

import org.scalatest.FunSuite
import offheap._

@data class L1(x: Byte, y: Byte)
@data class L2(x: Byte, y: Short)
@data class L3(x: Byte, y: Int)
@data class L4(x: Byte, y: Long)

class LayoutSuite extends FunSuite {
  implicit val alloc = Allocator()

  test("L1.x offset") { assert(offsetOf[L1]("x") == 0) }
  test("L2.x offset") { assert(offsetOf[L2]("x") == 0) }
  test("L3.x offset") { assert(offsetOf[L3]("x") == 0) }
  test("L4.x offset") { assert(offsetOf[L4]("x") == 0) }

  test("L1.y offset") { assert(offsetOf[L1]("y") == 1) }
  test("L2.y offset") { assert(offsetOf[L2]("y") == 2) }
  test("L3.y offset") { assert(offsetOf[L3]("y") == 4) }
  test("L4.y offset") { assert(offsetOf[L4]("y") == 8) }

  test("sizeOfData[L1]") { assert(sizeOfData[L1] == 2 ) }
  test("sizeOfData[L2]") { assert(sizeOfData[L2] == 4 ) }
  test("sizeOfData[L3]") { assert(sizeOfData[L3] == 8 ) }
  test("sizeOfData[L4]") { assert(sizeOfData[L4] == 16) }

  test("alignmentOfData[L1]") { assert(alignmentOfData[L1] == 1) }
  test("alignmentOfData[L2]") { assert(alignmentOfData[L2] == 2) }
  test("alignmentOfData[L3]") { assert(alignmentOfData[L3] == 4) }
  test("alignmentOfData[L4]") { assert(alignmentOfData[L4] == 8) }
}
