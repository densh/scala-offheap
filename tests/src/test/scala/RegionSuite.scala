package test

import org.scalatest.FunSuite
import offheap._, x64._

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
    intercept[InaccessibleRegionException] {
      d.value
    }
  }

  test("allocate after end") {
    var rr: Region = null
    Region { r =>
      rr = r
    }
    intercept[InaccessibleRegionException] {
      Dummy(10)(rr)
    }
  }

  test("close before end") {
    Region { r =>
      assert(r.isOpen)
      r.close
      assert(r.isClosed)
      intercept[InaccessibleRegionException] {
        Dummy(10)(r)
      }
    }
  }

  test("open close") {
    val r = Region.open
    assert(r.isOpen)
    r.close
    assert(r.isClosed)
  }
}
