package test

import org.scalatest.FunSuite
import offheap._

@data class Emb1(x: Int, y: Long)
@data class Emb2(x: Byte, @embed emb1: Emb1)

@data class Emb3(x: Int, y: Short)
@data class Emb4(x: Byte, @embed emb3: Emb3, y: Long)

class EmbedSuite extends FunSuite {

  test("Emb1.x    offset") { assert(offsetOf[Emb1]("x") == 0) }
  test("Emb1.y    offset") { assert(offsetOf[Emb1]("y") == 8) }

  test("Emb2.x    offset") { assert(offsetOf[Emb2]("x") == 0) }
  test("Emb2.emb1 offset") { assert(offsetOf[Emb2]("emb1") == 8) }

  test("Emb3.x    offset") { assert(offsetOf[Emb3]("x") == 0) }
  test("Emb3.y    offset") { assert(offsetOf[Emb3]("y") == 4) }

  test("Emb4.x    offset") { assert(offsetOf[Emb4]("x") == 0) }
  test("Emb4.emb3 offset") { assert(offsetOf[Emb4]("emb3") == 4) }
  test("Emb4.y    offset") { assert(offsetOf[Emb4]("y") == 16) }

  test("Emb1 data alignment") { assert(alignmentOfData[Emb1] == 8) }
  test("Emb2 data alignment") { assert(alignmentOfData[Emb2] == 8) }
  test("Emb3 data alignment") { assert(alignmentOfData[Emb3] == 4) }
  test("Emb4 data alignment") { assert(alignmentOfData[Emb4] == 8) }

  test("sizeOfData[Emb1]") { assert(sizeOfData[Emb1] == 16) }
  test("sizeOfData[Emb2]") { assert(sizeOfData[Emb2] == 24) }
  test("sizeOfData[Emb3]") { assert(sizeOfData[Emb3] == 6) }
  test("sizeOfData[Emb4]") { assert(sizeOfData[Emb4] == 24) }
}
