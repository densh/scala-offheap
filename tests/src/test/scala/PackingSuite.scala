package test

import java.{lang => jl}
import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import offheap.internal.Sanitizer._

class PackingSuite extends Properties("Packing") {
  val ADDR_MASK = jl.Long.MAX_VALUE >> 16
  val ids =
    for (id <- Gen.choose(0, 65535))
      yield id
  val addrs =
    for (l <- Gen.choose(jl.Long.MIN_VALUE, jl.Long.MAX_VALUE))
      yield l & ADDR_MASK
  property("pack/unpackId") = forAll(ids, addrs) { (id, addr) =>
    unpackId(pack(id, addr)) == id
  }
  property("pack/unpackAddr") = forAll(ids, addrs) { (id, addr) =>
    unpackAddr(pack(id, addr)) == addr
  }
}
