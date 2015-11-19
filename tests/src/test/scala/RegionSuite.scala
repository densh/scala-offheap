package test

import org.scalatest.FunSuite
import scala.offheap._

@data class Dummy(value: Int)

trait RegionSuite extends FunSuite {
  implicit val props: Region.Props

  test("allocate") {
    Region { r =>
      val d = Dummy(10)(r)
      assert(d.nonEmpty)
      assert(d.value == 10)
    }
  }

  ignore("access after end") {
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
    intercept[RegionClosedException] {
      Dummy(10)(rr)
    }
  }

  test("open close") {
    val r = Region.open
    assert(r.isOpen)
    r.close
    assert(!r.isOpen)
  }

  test("reallocate same") {
    Region { r =>
      val addr = r.allocate(32, alignment = 8)
      val naddr = r.reallocate(addr, 32, 32, alignment = 8)
      assert(addr == naddr)
    }
  }

  test("reallocate smaller") {
    Region { r =>
      val addr = r.allocate(32, alignment = 8)
      val naddr = r.reallocate(addr, 32, 16, alignment = 8)
      assert(addr == naddr)
    }
  }

  test("reallocate bigger") {
    Region { r =>
      val addr = r.allocate(32, alignment = 8)
      val naddr = r.reallocate(addr, 32, 64, alignment = 8)
      assert(addr != naddr)
    }
  }

  test("reallocate closed") {
    val r = Region.open
    val addr = r.allocate(32, alignment = 8)
    r.close
    intercept[RegionClosedException] {
      r.reallocate(addr, 32, 32, alignment = 8)
    }
  }

  test("free open") {
    Region { r =>
      val addr = r.allocate(32, alignment = 8)
      r.free(addr)
    }
  }

  test("free closed") {
    val r = Region.open
    val addr = r.allocate(32, alignment = 8)
    r.close
    intercept[RegionClosedException] {
      r.free(addr)
    }
  }
}

class PoolRegionSuite extends RegionSuite {
  implicit val props = Region.Props(Pool())
}
