package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class RegionClose {
  implicit val pool: Pool = Pool(Memory())
  var r: Region = _

  @Param(scala.Array("1024", "2048", "4096"))
  var allocatedPages: Int = _

  @Setup(Level.Invocation)
  def setup(): Unit = {
    r = Region.open
    for (_ <- 1 to allocatedPages)
      r.allocate(pool.pageSize)
  }

  @Benchmark
  def close = r.close()
}

@State(Scope.Thread)
class RegionOpen {
  implicit val pool: Pool = Pool(Memory())
  var r: Region = _

  @TearDown(Level.Invocation)
  def tearDown(): Unit = r.close

  @Benchmark
  def open = {
    r = Region.open
    r
  }
}

@State(Scope.Thread)
class RegionAllocateCurrent {
  implicit val pool: Pool = Pool(Memory())
  var r: Region = _

  @Setup(Level.Invocation)
  def setup(): Unit =
    r = Region.open

  @TearDown(Level.Invocation)
  def tearDown(): Unit = r.close()

  @Benchmark
  def allocate = r.allocate(16L)
}

@State(Scope.Thread)
class RegionAllocateNext {
  implicit val pool: Pool = Pool(Memory())
  var r: Region = _

  @Setup(Level.Invocation)
  def setup(): Unit = {
    r = Region.open
    r.allocate(pool.pageSize)
  }

  @TearDown(Level.Invocation)
  def tearDown(): Unit = r.close()

  @Benchmark
  def allocate = r.allocate(16L)
}



