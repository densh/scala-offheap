package test

import org.scalatest.FunSuite
import offheap._

@data class EPoint(x: Int, y: Int)
@data class EContainer(arr: EmbedArray[EPoint])
@data class ECell(var v: Int)

class EmbedArraySuite extends FunSuite {
  implicit val alloc = malloc

  test("uninit") {
    val arr = EmbedArray.uninit[EPoint](2)
    val p1 = arr(0)
    p1.x + p1.y
    val p2 = arr(1)
    p2.x + p2.y
  }

  test("vararg") {
    val arr = EmbedArray(EPoint(10, 20), EPoint(30, 40))
    assert(arr(0).x == 10)
    assert(arr(0).y == 20)
    assert(arr(1).x == 30)
    assert(arr(1).y == 40)
  }

  test("fill") {
    var i = 0
    val arr = EmbedArray.fill[EPoint](10) {
      val res = EPoint(i, i)
      i += 1
      res
    }
    for (j <- 0 to 9) {
      assert(arr(j).x == j)
      assert(arr(j).y == j)
    }
  }

  test("map") {
    val arr = EmbedArray(EPoint(10, 20), EPoint(30, 40))
    val narr = arr.map { p => p.copy(x = p.x * 2, y = p.y / 10) }
    assert(narr(0).x == 20)
    assert(narr(0).y == 2)
    assert(narr(1).x == 60)
    assert(narr(1).y == 4)
  }

  test("out of bounds") {
    val arr = EmbedArray.uninit[EPoint](10)
    intercept[IndexOutOfBoundsException] { arr(-1) }
    intercept[IndexOutOfBoundsException] { arr(10) }
  }

  test("copy") {
    val dst = EmbedArray.uninit[EPoint](3)
    val src = EmbedArray(EPoint(1, 2), EPoint(2, 3), EPoint(3, 4))
    EmbedArray.copy(src, 0, dst, 0, 3)
    for (i <- 0 to 2) {
      assert(dst(i).x == src(i).x)
      assert(dst(i).y == src(i).y)
    }
  }

  test("empty is empty") {
    EmbedArray.empty[EPoint].isEmpty
  }

  test("embed array as a field") {
    val container = EContainer(EmbedArray(EPoint(10, 20)))
    assert(container.arr.nonEmpty)
    assert(container.arr(0).x == 10)
    assert(container.arr(0).y == 20)
  }

  test("new data is copied over") {
    val cell = ECell(3)
    val arr = EmbedArray(cell)
    assert(arr(0).v == 3)
    cell.v = 4
    assert(arr(0).v == 3)
    arr(0) = cell
    assert(arr(0).v == 4)
  }
}
