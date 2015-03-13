package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Array(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  lazy val A = c.prefix.tree.tpe.baseType(ArrayClass).typeArgs.head

  def isEmpty = q"${c.prefix.tree}.$ref == null"

  def nonEmpty = q"${c.prefix.tree}.$ref != null"

  def size = stabilized(c.prefix.tree) { pre =>
    read(q"$pre.$ref.addr", SizeTpe, q"$pre.$ref.memory")
  }

  def boundsChecked(index: Tree)(ifOk: Tree => Tree => Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(index) { idx =>
        val size = fresh("size")
        q"""
          val $size: $SizeTpe = ${read(q"$pre.$ref.addr", AddrTpe, q"$pre.$ref.memory")}
          if ($idx >= 0 && $idx < $size)  ${ifOk(pre)(idx)}
          else throw new _root_.java.lang.IndexOutOfBoundsException($index.toString)
        """
      }
    }

  def apply(index: Tree) =
    boundsChecked(index) { pre => i =>
      val addr = fresh("addr")
      val mem  = fresh("mem")
      q"""
        val $mem = $pre.$ref.memory
        val $addr = $pre.$ref.addr + $MemoryModule.sizeof[$SizeTpe] + $i * $MemoryModule.sizeof[$A]
        ${read(q"$addr", A, q"$mem")}
      """
    }

  def update(index: Tree, value: Tree) =
    boundsChecked(index) { pre => i =>
      val addr = fresh("addr")
      val mem  = fresh("mem")
      q"""
        val $mem = $pre.$ref.memory
        val $addr = $pre.$ref.addr + $MemoryModule.sizeof[$SizeTpe] + $i * $MemoryModule.sizeof[$A]
        ${write(q"$addr", A, value, q"$mem")}
      """
    }

  def foreach(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mem = fresh("mem")
      q"""
        if ($pre.$ref != null) {
          val $mem = $pre.$ref.memory
          ${iterate(A, pre, q"$mem", p => app(f, read(p, A, q"$mem")))}
        }
      """
    }

  def iterate(T: Type, pre: Tree, mem: Tree, f: Tree => Tree) = {
    val p = fresh("p")
    val len = fresh("len")
    val step = fresh("step")
    val bound = fresh("bound")
    q"""
      var $p: $AddrTpe = $pre.$ref.addr
      val $len: $SizeTpe = ${read(q"$p", SizeTpe, mem)}
      $p += $MemoryModule.sizeof[$SizeTpe]
      val $step: $SizeTpe = $MemoryModule.sizeof[$T]
      val $bound: $AddrTpe = $p + $len * $step
      while ($p < $bound) {
        ${f(q"$p")}
        $p += $step
      }
    """
  }

  def map[B: WeakTypeTag](f: Tree)(m: Tree) = {
    val B = wt[B]
    assertAllocatable(B)
    stabilized(c.prefix.tree) { pre =>
      stabilized(m) { mem =>
        val narr = fresh("narr")
        val v    = fresh("v")
        val p    = fresh("p")
        val step = fresh("step")
        q"""
          val $narr = $ArrayModule.uninit[$B]($pre.length)($mem)
          val $step = $MemoryModule.sizeof[$B]
          var $p    = $narr.$ref.addr + $MemoryModule.sizeof[$SizeTpe]
          $pre.foreach { $v: $A =>
            ${write(q"$p", B, app(f, q"$v"), mem)}
            $p += $step
          }
          $narr
        """
      }
    }
  }

  def uninit[T: WeakTypeTag](n: Tree)(m: Tree) = {
    val T = wt[T]
    assertAllocatable(T)
    stabilized(n) { len =>
      stabilized(m) { mem =>
        val addr = fresh("addr")
        val size = q"$MemoryModule.sizeof[$AddrTpe] + $len * $MemoryModule.sizeof[$T]"
        q"""
          if ($len < 0) throw new $IllegalArgumentExceptionClass
          else if ($len == 0) $ArrayModule.empty[$T]
          else {
            val $addr = $mem.allocate($size)
            ${write(q"$addr", SizeTpe, len, mem)}
            $ArrayModule.fromRef[$T](new $RefClass($addr, $mem))
          }
        """
      }
    }
  }

  def vararg[T: WeakTypeTag](values: Tree*)(m: Tree) = {
    val T = wt[T]
    assertAllocatable(T, s"Can't allocate offheap array of $T")
    if (values.isEmpty)
      q"$ArrayModule.empty[$T]"
    else stabilized(m) { mem =>
      val arr    = fresh("arr")
      val addr   = fresh("adr")
      val step   = fresh("step")
      val writes = values.zipWithIndex.map { case (v, i) =>
        write(q"$addr + $i * $step", T, v, mem)
      }
      q"""
        val $arr = $ArrayModule.uninit[$T](${values.length})($mem)
        val $step = $MemoryModule.sizeof[$T]
        val $addr = $arr.$ref.addr + $MemoryModule.sizeof[$AddrTpe]
        ..$writes
        $arr
      """
    }
  }

  def fill[T: WeakTypeTag](n: Tree)(elem: Tree)(m: Tree) = {
    stabilized(n) { len =>
      stabilized(m) { mem =>
        val T   = wt[T]
        val arr = fresh("arr")
        q"""
          val $arr = $ArrayModule.uninit[$T]($len)
          ${iterate(T, q"$arr", mem, p => write(p, T, elem, mem))}
          $arr
        """
      }
    }
  }

  // TODO: bounds checks
  def copy[T: WeakTypeTag](from: Tree, fromIndex: Tree,
                           to: Tree, toIndex: Tree, size: Tree) =
    stabilized(from) { from =>
      stabilized(to) { to =>
        val T = wt[T]
        def offset(idx: Tree) =
          q"$MemoryModule.sizeof[$SizeTpe] + $idx * $MemoryModule.sizeof[$T]"
        q"""
          if ($to.$ref.memory ne $from.$ref.memory)
            throw new $IllegalArgumentExceptionClass(
              "copy between different memories is not supported")
          $to.$ref.memory.copy($from.$ref.addr + ${offset(fromIndex)},
                               $to.$ref.addr + ${offset(toIndex)},
                               $size * $MemoryModule.sizeof[$T])
        """
      }
    }
}
