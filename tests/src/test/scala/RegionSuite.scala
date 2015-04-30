package test

import org.scalatest.FunSuite
import offheap._

@data class Dummy(value: Int)

class RegionSuite extends FunSuite {
  implicit val pool = Pool(Memory())

  test("allocate") {
    println("a.1")
    Region { r =>
      println("b.1")
      val d = Dummy(10)(r)
      println("b.2")
      assert(d.nonEmpty)
      println("b.3")
      assert(d.value == 10)
      println("b.4")
    }
    println("a.2")
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
