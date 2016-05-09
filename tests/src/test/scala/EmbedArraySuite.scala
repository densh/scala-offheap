package test

import org.scalatest.FunSuite
import scala.offheap._

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

  test("transform") {
    val arr = EmbedArray(EPoint(10, 20), EPoint(30, 40))
    val narr = arr.transform { p => p.copy(x = p.x * 2, y = p.y / 10) }
    assert(narr == arr)
    assert(arr(0).x == 20)
    assert(arr(0).y == 2)
    assert(arr(1).x == 60)
    assert(arr(1).y == 4)
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


  test("filter") {
    val arr = EmbedArray(
      EPoint(1, 10),
      EPoint(2, 20),
      EPoint(3, 30),
      EPoint(4, 40),
      EPoint(5, 50),
      EPoint(7, 70),
      EPoint(9, 90)
    )
    val narr = arr.filter(p => p.x % 2 == 0)
    assert(narr.nonEmpty)
    assert(narr.size == 2)
    assert(narr(0).x == 2)
    assert(narr(0).y == 20)
    assert(narr(1).x == 4)
    assert(narr(1).y == 40)

    val narr2 = arr.filter(p => p.x == 3)
    assert(narr2.nonEmpty)
    assert(narr2.size == 1)
    assert(narr2(0).x == 3)
    assert(narr2(0).y == 30)
  }

  test("filter no matching predicate") {
    val arr = EmbedArray(
      EPoint(1, 10),
      EPoint(2, 20),
      EPoint(3, 30),
      EPoint(4, 40)
    )
    val narr = arr.filter(p => p.x > 10)
    assert(narr.isEmpty)
    assert(narr.length == 0)
  }

  test("filter empty") {
    assert(EmbedArray.empty[EPoint].filter(_ => true).isEmpty)
    assert(EmbedArray.empty[EPoint].filter(_ => false).isEmpty)
  }

  test("foldLeft") {
    val arr = EmbedArray(
      EPoint(1, 10),
      EPoint(2, 20),
      EPoint(3, 30)
    )
    assert(arr.foldLeft[Int](0)((acc, el) => (acc + el.x) * el.x) == 27)
  }

  test("foldLeft empty") {
    assert(EmbedArray.empty[EPoint].foldLeft[Int](3)((_, _) => 1) == 3)
  }

  test("foldRight") {
    val arr = EmbedArray(
      EPoint(1, 10),
      EPoint(2, 20),
      EPoint(3, 30)
    )
    assert(arr.foldRight[Int](0)((el, acc) => (acc + el.x) * el.x) == 23)
  }

  test("foldRight empty") {
    assert(EmbedArray.empty[EPoint].foldRight[Int](5)((_, _) => 1) == 5)
  }

  test("reduceLeft") {
    val arr = EmbedArray[EPoint](
      EPoint(1, 10),
      EPoint(2, 20),
      EPoint(3, 30)
    )
    val result = arr.reduceLeft { (acc, el) => EPoint((acc.x + el.x) * el.x, acc.y) }
    assert(result.x == 27)

    intercept[UnsupportedOperationException] {
      Array.empty[EPoint].reduceLeft { (_, _) => EPoint(0, 0) }
    }
  }

  test("reduceRight") {
    val arr = EmbedArray[EPoint](
      EPoint(1, 10),
      EPoint(2, 20),
      EPoint(3, 30)
    )
    val result = arr.reduceRight { (el, acc) => EPoint((acc.x + el.x) * el.x, acc.y) }
    assert(result.x == 11)

    intercept[UnsupportedOperationException] {
      Array.empty[EPoint].reduceRight { (_, _) => EPoint(0, 0) }
    }
  }

  test("forall") {
    val arr = EmbedArray(
      EPoint(1, 10),
      EPoint(3, 30),
      EPoint(5, 50),
      EPoint(7, 70)
    )

    assert(arr.forall(p => p.x < 10))
    assert(arr.forall(p => p.x % 2 == 1))
    assert(!arr.forall(p => p.x < 6))
  }

  test("forall on empty array returns true") {
    assert(EmbedArray.empty[EPoint].forall(p => p.x < 10))
    assert(EmbedArray.empty[EPoint].forall(_ => false))
  }

  test("exists") {
    val arr = EmbedArray(
      EPoint(1, 10),
      EPoint(3, 30),
      EPoint(5, 50),
      EPoint(7, 70)
    )

    assert(arr.exists(p => p.y <= 10))
    assert(arr.exists(p => p.x == 5))
    assert(!arr.exists(p => p.x % 2 == 0))
  }

  test("exists on empty array returns false") {
    assert(!EmbedArray.empty[EPoint].exists(p => p.y < 10))
    assert(!EmbedArray.empty[EPoint].exists(_ => true))
  }
}
