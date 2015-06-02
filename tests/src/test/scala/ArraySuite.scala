package test

import org.scalatest.FunSuite
import offheap._

@data class ArrayContainer(var arr: Array[Int])

class ArraySuite extends FunSuite {
  implicit val alloc = malloc

  test("uninit") {
    val arr = Array.uninit[Int](10)
    assert(arr.nonEmpty)
    assert(arr.size == 10)
  }

  test("uninit empty") {
    assert(Array.uninit[Int](0).isEmpty)
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

  test("fill empty") {
    assert(Array.fill(0)(0).isEmpty)
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

  test("map empty") {
    assert(Array.empty[Int].map(_ * 2).isEmpty)
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

  test("copy out of bounds") {
    val arr1 = Array(0, 0, 0)
    val arr2 = Array(1, 1, 1, 1, 1, 1, 1, 1)
    intercept[IndexOutOfBoundsException] {
      Array.copy(arr2, 1, arr1, 2, 3)
    }
  }

  test("copy empty") {
    val arr1 = Array(0, 0, 0, 0)
    val arr2 = Array.empty[Int]
    intercept[IllegalArgumentException] { Array.copy(arr2, 0, arr1, 0, 4) }
    intercept[IllegalArgumentException] { Array.copy(arr1, 0, arr2, 0, 4) }
  }

  test("arrays can be fields in data classes") {
    val arr1 = Array(1, 2, 3)
    val arr2 = Array(2, 3, 4)
    val cont = ArrayContainer(arr1)
    assert(cont.arr == arr1)
    cont.arr = arr2
    assert(cont.arr == arr2)
  }

  test("empty array is empty") {
    assert(Array.empty[Int].isEmpty)
    assert(!Array.empty[Int].nonEmpty)
  }

  test("non-empty array is not empty") {
    assert(Array(1).nonEmpty)
    assert(!Array(1).isEmpty)
  }

  test("offheap to onheap") {
    val arr = Array(1, 2, 3, 4, 5)
    val jarr = arr.toArray
    assert(jarr.length == 5)
    val jarr2 = scala.Array(1, 2, 3, 4, 5)
    for (i <- 0 to 4)
      assert(jarr(i) == jarr2(i))
  }

  test("empty offheap to onheap") {
    assert(Array.empty[Int].toArray.isEmpty)
  }

  test("onheap to offheap") {
    val jarr = scala.Array(1, 2, 3, 4, 5)
    val arr = Array.fromArray(jarr)
    assert(arr.length == 5)
    val arr2 = Array(1, 2, 3, 4, 5)
    for (i <- 0 to 4)
      assert(arr(i) == arr2(i))
  }

  test("empty onheap to offheap") {
    assert(Array.fromArray(scala.Array.empty[Int]).isEmpty)
  }

  test("clone") {
    val arr = Array(1, 2, 3, 4, 5)
    val arr2 = arr.clone
    assert(arr2.size == 5)
    for (i <- 0 to 4)
      assert(arr(i) == arr(i))
  }

  test("clone empty") {
    assert(Array.empty[Int].clone.isEmpty)
  }
}
