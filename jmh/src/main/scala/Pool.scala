package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class PoolContention {
  @Param(Array("linked", /*"caslinked",*/ "stack"))
  var allocator: String = _

  @Benchmark
  def contention = {
    val r = allocator match {
      case "linked"    => new internal.LinkedRegion
      case "caslinked" => new internal.CASLinkedRegion
      case "stack"     => new internal.AddrStackRegion
    }
    r.close()
  }
}
