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
  implicit val alloc = malloc

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

  test("sizeOfEmbed[L1]") { assert(sizeOfEmbed[L1] == 2 ) }
  test("sizeOfEmbed[L2]") { assert(sizeOfEmbed[L2] == 4 ) }
  test("sizeOfEmbed[L3]") { assert(sizeOfEmbed[L3] == 8 ) }
  test("sizeOfEmbed[L4]") { assert(sizeOfEmbed[L4] == 16) }
  test("sizeOfEmbed[L5]") { assert(sizeOfEmbed[L5] == 16) }
  test("sizeOfEmbed[L6]") { assert(sizeOfEmbed[L6] == 24) }
  test("sizeOfEmbed[L7]") { assert(sizeOfEmbed[L7] == 6) }
  test("sizeOfEmbed[L8]") { assert(sizeOfEmbed[L8] == 24) }
  test("sizeOfEmbed[L9]") { assert(sizeOfEmbed[L9] == 1) }

  test("alignmentOfEmbed[L1]") { assert(alignmentOfEmbed[L1] == 1) }
  test("alignmentOfEmbed[L2]") { assert(alignmentOfEmbed[L2] == 2) }
  test("alignmentOfEmbed[L3]") { assert(alignmentOfEmbed[L3] == 4) }
  test("alignmentOfEmbed[L4]") { assert(alignmentOfEmbed[L4] == 8) }
  test("alignmentOfEmbed[L5]") { assert(alignmentOfEmbed[L5] == 8) }
  test("alignmentOfEmbed[L6]") { assert(alignmentOfEmbed[L6] == 8) }
  test("alignmentOfEmbed[L7]") { assert(alignmentOfEmbed[L7] == 4) }
  test("alignmentOfEmbed[L8]") { assert(alignmentOfEmbed[L8] == 8) }
  test("alignmentOfEmbed[L9]") { assert(alignmentOfEmbed[L9] == 1) }
}
