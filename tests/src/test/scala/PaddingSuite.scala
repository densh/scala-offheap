package test

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scala.offheap.Addr
import scala.offheap.internal.pad

class PaddingSuite extends Properties("Padding") {
  property("2") = forAll { (addr: Addr) =>
    pad(addr, 2) % 2 == 0
  }

  property("4") = forAll { (addr: Addr) =>
    pad(addr, 4) % 4 == 0
  }

  property("8") = forAll { (addr: Addr) =>
    pad(addr, 8) % 8 == 0
  }

  property("16") = forAll { (addr: Addr) =>
    pad(addr, 16) % 16 == 0
  }

  property("32") = forAll { (addr: Addr) =>
    pad(addr, 32) % 32 == 0
  }

  property("64") = forAll { (addr: Addr) =>
    pad(addr, 64) % 64 == 0
  }

  property("128") = forAll { (addr: Addr) =>
    pad(addr, 64) % 64 == 0
  }
}
