package test

import org.scalatest.FunSuite
import offheap._

@data class AS1(a: Long, b: Byte)
@data class AS2(a: Int, b: Int)
@data class AS3(a: Byte, b: Short)

class ArrayStrideSuite extends FunSuite {
  test("strideOf[Byte]")   { assert(strideOf[Byte]   == 1) }
  test("strideOf[Char]")   { assert(strideOf[Char]   == 2) }
  test("strideOf[Short]")  { assert(strideOf[Short]  == 2) }
  test("strideOf[Int]")    { assert(strideOf[Int]    == 4) }
  test("strideOf[Long]")   { assert(strideOf[Long]   == 8) }
  test("strideOf[Float]")  { assert(strideOf[Float]  == 4) }
  test("strideOf[Double]") { assert(strideOf[Double] == 8) }
  test("strideOf[AS1]")    { assert(strideOf[AS1]    == 8) }

  test("strideOfEmbed[AS1]") { assert(strideOfEmbed[AS1] == 16) }
  test("strideOfEmbed[AS2]") { assert(strideOfEmbed[AS2] == 8)  }
  test("strideOfEmbed[AS3]") { assert(strideOfEmbed[AS3] == 4)  }
}
