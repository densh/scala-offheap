package test

import org.scalatest.FunSuite

import scala.offheap._

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

  test("transform") {
    val arr = Array(1, 2, 3, 4)
    val narr = arr.transform(v => v * 2)
    val exp = Array(2, 4, 6, 8)
    assert(arr == narr)
    assert(narr.sameElements(exp))
  }

  test("transform empty") {
    assert(Array.empty[Int].transform(_ * 2).isEmpty)
  }

  test("read out of bounds") {
    val arr = Array(1, 2, 3, 4)
    intercept[IndexOutOfBoundsException] { arr(-1) }
    intercept[IndexOutOfBoundsException] { arr(4)  }
  }

  test("write out of bounds") {
    val arr = Array(1, 2, 3, 4)
    intercept[IndexOutOfBoundsException] { arr(-1) = 42 }
    intercept[IndexOutOfBoundsException] { arr(4) = 42 }
  }

  test("empty read out of bounds") {
    intercept[IndexOutOfBoundsException] {
      Array.empty[Int].apply(0)
    }
  }

  test("empty write out of bounds") {
    intercept[IndexOutOfBoundsException] {
      Array.empty[Int].update(0, 42)
    }
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

  test("empty size") {
    assert(Array.empty[Int].size == 0)
  }

  test("filter") {
    val arr = Array(1, 2, 3, 4, 5, 7, 9)
    val narr = arr.filter(x => x % 2 == 0)
    assert(narr.nonEmpty)
    assert(narr.size == 2)
    assert(narr(0) == 2)
    assert(narr(1) == 4)

    val narr2 = arr.filter(x => x == 3)
    assert(narr2.nonEmpty)
    assert(narr2.size == 1)
    assert(narr2(0) == 3)
  }

  test("filter no matching predicate") {
    val arr = Array(1, 2, 3, 4)

    val narr = arr.filter(x => x > 10)
    assert(narr.isEmpty)
    assert(narr.length == 0)
  }

  test("filter empty") {
    assert(Array.empty[Int].filter(_ => true).isEmpty)
    assert(Array.empty[Int].filter(_ => false).isEmpty)
  }

  test("forall") {
    val arr = Array(1, 3, 5, 7)
    assert(arr.forall(_ < 10))
    assert(arr.forall(x => x % 2 == 1))
    assert(!arr.forall(_ < 6))
  }

  test("forall on empty array returns true") {
    assert(Array.empty[Int].forall(_ < 10))
    assert(Array.empty[Double].forall(_ => false))
  }

  test("exists") {
    val arr = Array(1, 3, 5, 7)
    assert(arr.exists(_ < 10))
    assert(arr.exists(_ == 5))
    assert(!arr.exists(x => x % 2 == 0))
  }

  test("exists on empty array returns false") {
    assert(!Array.empty[Int].exists(_ < 10))
    assert(!Array.empty[Double].exists(_ => true))
  }

  test("sameElements") {
    var arr1 = Array(1, 3, 5, 7)
    val arr2 = Array(1, 3, 5, 7)
    val arr3 = Array(1, 3, 5, 8)
    val arr4 = Array(1, 3, 5)

    assert(arr1.sameElements(arr1))
    assert(arr1.sameElements(arr2))
    assert(!arr1.sameElements(arr3))
    assert(!arr1.sameElements(arr4))
    assert(!arr1.sameElements(Array.empty[Int]))
    assert(!Array.empty[Int].sameElements(arr1))
    assert(Array.empty[Int].sameElements(Array.empty[Int]))
  }
}
