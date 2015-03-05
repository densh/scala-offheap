package test

import org.scalatest.FunSuite
import offheap._, x64._

@offheap case class Point(x: Double, y: Double) {
  def distanceTo(other: Point): Double =
    math.sqrt(math.pow(other.x - x, 2) + math.pow(other.y - y, 2))
}

class PointSuite extends FunSuite {
  implicit val pool = Pool(UnsafeMemory)

  test("accessors") {
    Region { r =>
      val p = Point(10, 20)(r)
      assert(p.x == 10)
      assert(p.y == 20)
    }
  }

  test("acessors on empty throw null reference exception") {
    intercept[NullPointerException] {
      Point.empty.x
    }
    intercept[NullPointerException] {
      Point.empty.y
    }
  }

  test("distance to") {
    Region { r =>
      val p1 = Point(0, 3)(r)
      val p2 = Point(4, 0)(r)
      assert(p1.distanceTo(p2) == 5.0d)
    }
  }

  test("distance to on empty throws null reference exception") {
    intercept[NullPointerException] {
      Point.empty.distanceTo(Point.empty)
    }
  }

  test("allocated value is not empty") {
    Region { r =>
      val p = Point(0, 0)(r)
      assert(p.nonEmpty)
      assert(!p.isEmpty)
    }
  }

  test("null asInstanceOf Point is empty") {
    assert(Point.empty.isEmpty)
    assert(!Point.empty.nonEmpty)
  }

  test("unapply") {
    Region { r =>
      val Point(x, y) = Point(10, 20)(r)
      assert(x == 10)
      assert(y == 20)
    }
  }

  test("unapply on empty throws MatchError") {
    intercept[MatchError] {
      val Point(_, _) = Point.empty
    }
  }

  test("matching empty point") {
    val (__ @ Point.empty) = Point.empty
  }

  test("copy") {
    Region { r =>
      val p = Point(10, 20)(r)
      val p2 = p.copy(x = 1)(r)
      assert(p2.x == 1)
      assert(p2.y == 20)
      val p3 = p.copy(y = 2)(r)
      assert(p3.x == 10)
      assert(p3.y == 2)
      val p4 = p.copy(x = 0, y = 0)(r)
      assert(p4.x == 0)
      assert(p4.x == 0)
    }
  }

  test("copy on null") {
    intercept[NullPointerException] {
      Region { r =>
        Point.empty.copy(x = 10)(r)
      }
    }
  }

  test("toString") {
    Region { r =>
      assert(Point(10, 20)(r).toString == "Point(10.0, 20.0)")
    }
  }

  test("toString on null") {
    intercept[NullPointerException] {
      Point.empty.toString
    }
  }

  test("equality") {
    Region { r =>
      val p1 = Point(10, 10)(r)
      val p2 = Point(10, 10)(r)
      assert(p1 != p2)
      assert(p1 == p1 && p2 == p2)
      assert(p1 != Point.empty)
      assert(p2 != Point.empty)
      assert(Point.empty == Point.empty)
    }
  }
}
