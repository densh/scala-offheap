package offheap.test.jmh

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import offheap._

@State(Scope.Thread)
class ArrayBench {
  val arr: offheap.Array[Int] = Array(1, 2, 3)

  @Benchmark
  def access = arr(0)
}
