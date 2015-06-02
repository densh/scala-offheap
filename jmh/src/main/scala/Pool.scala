package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class PoolContention {
  implicit val policy: Region.Policy = PoolRegion.Policy()

  @Benchmark
  def contention = {
    val r = Region.open
    r.close
  }
}
