package test

import org.scalatest.FunSuite
import offheap._

@data class L1(x: Byte, y: Byte)
@data class L2(x: Byte, y: Short)
@data class L3(x: Byte, y: Int)
@data class L4(x: Byte, y: Long)

@data class L5(x: Int, y: Long)
@data class L6(x: Byte, @embed emb: L5)
@data class L7(x: Int, y: Short)
@data class L8(x: Byte, @embed emb: L7, y: Long)

@data class L9

class LayoutSuite extends FunSuite {
  implicit val alloc = Allocator()

  test("L1.x offset") { assert(offsetOf[L1]("x") == 0) }
  test("L2.x offset") { assert(offsetOf[L2]("x") == 0) }
  test("L3.x offset") { assert(offsetOf[L3]("x") == 0) }
  test("L4.x offset") { assert(offsetOf[L4]("x") == 0) }
  test("L5.x offset") { assert(offsetOf[L5]("x") == 0) }
  test("L6.x offset") { assert(offsetOf[L6]("x") == 0) }
  test("L7.x offset") { assert(offsetOf[L7]("x") == 0) }
  test("L8.x offset") { assert(offsetOf[L8]("x") == 0) }

  test("L1.y offset") { assert(offsetOf[L1]("y") == 1) }
  test("L2.y offset") { assert(offsetOf[L2]("y") == 2) }
  test("L3.y offset") { assert(offsetOf[L3]("y") == 4) }
  test("L4.y offset") { assert(offsetOf[L4]("y") == 8) }
  test("L5.y offset") { assert(offsetOf[L5]("y") == 8) }
  test("L7.y offset") { assert(offsetOf[L7]("y") == 4) }
  test("L8.y offset") { assert(offsetOf[L8]("y") == 16) }

  test("L6.emb offset") { assert(offsetOf[L6]("emb") == 8) }
  test("L8.emb offset") { assert(offsetOf[L8]("emb") == 4) }

  test("sizeOfData[L1]") { assert(sizeOfData[L1] == 2 ) }
  test("sizeOfData[L2]") { assert(sizeOfData[L2] == 4 ) }
  test("sizeOfData[L3]") { assert(sizeOfData[L3] == 8 ) }
  test("sizeOfData[L4]") { assert(sizeOfData[L4] == 16) }
  test("sizeOfData[L5]") { assert(sizeOfData[L5] == 16) }
  test("sizeOfData[L6]") { assert(sizeOfData[L6] == 24) }
  test("sizeOfData[L7]") { assert(sizeOfData[L7] == 6) }
  test("sizeOfData[L8]") { assert(sizeOfData[L8] == 24) }
  test("sizeOfData[L9]") { assert(sizeOfData[L9] == 1) }

  test("alignmentOfData[L1]") { assert(alignmentOfData[L1] == 1) }
  test("alignmentOfData[L2]") { assert(alignmentOfData[L2] == 2) }
  test("alignmentOfData[L3]") { assert(alignmentOfData[L3] == 4) }
  test("alignmentOfData[L4]") { assert(alignmentOfData[L4] == 8) }
  test("alignmentOfData[L5]") { assert(alignmentOfData[L5] == 8) }
  test("alignmentOfData[L6]") { assert(alignmentOfData[L6] == 8) }
  test("alignmentOfData[L7]") { assert(alignmentOfData[L7] == 4) }
  test("alignmentOfData[L8]") { assert(alignmentOfData[L8] == 8) }
  test("alignmentOfData[L9]") { assert(alignmentOfData[L9] == 1) }
}
