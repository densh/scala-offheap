import org.scalatest.FunSuite
import offheap._

@offheap class Point(x: Double, y: Double) {
  def distanceTo(other: Point): Double =
    math.sqrt(math.pow(other.x - x, 2) + math.pow(other.y - y, 2))
}

class OffheapClassSuite extends FunSuite {
  test("accessors") {
    Region { implicit r =>
      val p = Point(10, 20)
      assert(p.x == 10)
      assert(p.y == 20)
    }
  }

  test("acessors on empty throw null reference exception") {
    intercept[NullRefException.type] {
      Point.empty.x
    }
    intercept[NullRefException.type] {
      Point.empty.y
    }
  }

  test("distance to") {
    Region { implicit r =>
      val p1 = Point(0, 3)
      val p2 = Point(4, 0)
      assert(p1.distanceTo(p2) == 5.0d)
    }
  }

  test("distance to on empty throws null reference exception") {
    intercept[NullRefException.type] {
      Point.empty.distanceTo(Point.empty)
    }
  }

  test("allocated value is not empty") {
    Region { implicit r =>
      val p = Point(0, 0)
      assert(p.nonEmpty)
      assert(!p.isEmpty)
    }
  }

  test("null asInstanceOf Point is empty") {
    assert(Point.empty.isEmpty)
    assert(!Point.empty.nonEmpty)
  }

  test("unapply") {
    Region { implicit r =>
      val Point(x, y) = Point(10, 20)
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
    Region { implicit r =>
      val p = Point(10, 20)
      val p2 = p.copy(x = 1)
      assert(p2.x == 1)
      assert(p2.y == 20)
      val p3 = p.copy(y = 2)
      assert(p3.x == 10)
      assert(p3.y == 2)
      val p4 = p.copy(x = 0, y = 0)
      assert(p4.x == 0)
      assert(p4.x == 0)
    }
  }

  test("copy on null") {
    intercept[NullRefException.type] {
      Region { implicit r =>
        Point.empty.copy(x = 10)
      }
    }
  }

  test("toString") {
    Region { implicit r =>
      assert(Point(10, 20).toString == "Point(10.0, 20.0)")
    }
  }

  test("toString on null") {
    intercept[NullRefException.type] {
      Point.empty.toString
    }
  }

  test("equality") {
    Region { implicit r =>
      val p1 = Point(10, 10)
      val p2 = Point(10, 10)
      assert(p1 != p2)
      assert(p1 == p1 && p2 == p2)
      assert(p1 != Point.empty)
      assert(p2 != Point.empty)
      assert(Point.empty == Point.empty)
    }
  }
}
