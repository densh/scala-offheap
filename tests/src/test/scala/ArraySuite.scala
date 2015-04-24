package test

import org.scalatest.FunSuite
import offheap._, x64._

@data class ArrayContainer(var arr: Array[Int])

class ArraySuite extends FunSuite {
  implicit val memory = Memory()

  test("uninit") {
    val arr = Array.uninit[Int](10)
    assert(arr.nonEmpty)
    assert(arr.size == 10)
  }

  test("vararg") {
    var arr = Array(1, 2, 3, 4)
    assert(arr.nonEmpty)
    assert(arr.size == 4)
    assert(arr(0) == 1)
    assert(arr(1) == 2)
    assert(arr(2) == 3)
    assert(arr(3) == 4)
  }

  test("fill") {
    var arr = Array.fill(10)(42)
    assert(arr.nonEmpty)
    assert(arr.size == 10)
    arr.foreach { v => assert(v == 42) }
  }

  test("map") {
    val arr = Array(1, 2, 3, 4)
    val narr = arr.map(v => (v + 1).toLong)
    assert(narr.nonEmpty)
    assert(narr.size == 4)
    assert(narr(0) == 2L)
    assert(narr(1) == 3L)
    assert(narr(2) == 4L)
    assert(narr(3) == 5L)
  }

  test("out of bounds") {
    val arr = Array(1, 2, 3, 4)
    intercept[IndexOutOfBoundsException] { arr(-1) }
    intercept[IndexOutOfBoundsException] { arr(4)  }
  }

  test("copy") {
    val arr1 = Array(0, 0, 0, 0, 0, 0, 0, 0)
    val arr2 = Array(1, 1, 1, 1, 1, 1, 1, 1)
    Array.copy(arr2, 1, arr1, 2, 3)
    val arr3 = Array(0, 0, 1, 1, 1, 0, 0, 0)
    for (i <- 0 to 7)
      assert(arr1(i) == arr3(i))
  }

  test("arrays can be fields in data classes") {
    val arr1 = Array(1, 2, 3)
    val arr2 = Array(2, 3, 4)
    val cont = ArrayContainer(arr1)
    assert(cont.arr == arr1)
    cont.arr = arr2
    assert(cont.arr == arr2)
  }
}
