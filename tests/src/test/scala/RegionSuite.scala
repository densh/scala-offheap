package test

import org.scalatest.FunSuite
import offheap._

@data class Dummy(value: Int)

class RegionSuite extends FunSuite {
  implicit val pool = Pool(Memory())

  test("allocate") {
    Region { r =>
      val d = Dummy(10)(r)
      assert(d.nonEmpty)
      assert(d.value == 10)
    }
  }

  test("access after end") {
    var d = Dummy.empty
    Region { r =>
      d = Dummy(10)(r)
    }
    intercept[InaccessibleMemoryException] {
      d.value
    }
  }

  test("allocate after end") {
    var rr: Region = null
    Region { r =>
      rr = r
    }
    intercept[IllegalArgumentException] {
      Dummy(10)(rr)
    }
  }

  test("open close") {
    val r = Region.open
    assert(r.isOpen)
    r.close
    assert(r.isClosed)
  }
}
