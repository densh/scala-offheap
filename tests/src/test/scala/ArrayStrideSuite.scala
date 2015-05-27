package test

import org.scalatest.FunSuite
import offheap._

@data class AS1(a: Long, b: Byte)
@data class AS2(a: Int, b: Int)
@data class AS3(a: Byte, b: Short)

class ArrayStrideSuite extends FunSuite {
  test("strideOf[Array[Byte]]")   { assert(strideOf[Array[Byte]]   == 1) }
  test("strideOf[Array[Char]]")   { assert(strideOf[Array[Char]]   == 2) }
  test("strideOf[Array[Short]]")  { assert(strideOf[Array[Short]]  == 2) }
  test("strideOf[Array[Int]]")    { assert(strideOf[Array[Int]]    == 4) }
  test("strideOf[Array[Long]]")   { assert(strideOf[Array[Long]]   == 8) }
  test("strideOf[Array[Float]]")  { assert(strideOf[Array[Float]]  == 4) }
  test("strideOf[Array[Double]]") { assert(strideOf[Array[Double]] == 8) }

  test("strideOf[EmbedArray[AS1]]") { assert(strideOf[EmbedArray[AS1]] == 16) }
  test("strideOf[EmbedArray[AS2]]") { assert(strideOf[EmbedArray[AS2]] == 8)  }
  test("strideOf[EmbedArray[AS3]]") { assert(strideOf[EmbedArray[AS3]] == 4)  }
}
