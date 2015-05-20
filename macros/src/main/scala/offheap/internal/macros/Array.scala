package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Array(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  lazy val A = c.prefix.tree.tpe.baseType(ArrayClass).typeArgs.head

  def isEmpty = q"${c.prefix.tree}.$addr == 0L"

  def nonEmpty = q"${c.prefix.tree}.$addr != 0L"

  def size = stabilized(c.prefix.tree) { pre =>
    read(q"$pre.$addr", SizeTpe)
  }

  def boundsChecked(index: Tree)(ifOk: Tree => Tree => Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(index) { idx =>
        val size = fresh("size")
        val happy = ifOk(pre)(idx)
        q"""
          if ($CHECKED) {
            val $size: $SizeTpe = ${read(q"$pre.$addr", AddrTpe)}
            if ($idx >= 0 && $idx < $size) $happy
            else throw new _root_.java.lang.IndexOutOfBoundsException($index.toString)
          } else $happy
        """
      }
    }

  def apply(index: Tree) =
    boundsChecked(index) { pre => i =>
      val naddr = fresh("addr")
      q"""
        val $naddr = $pre.$addr + $offheap.sizeOf[$SizeTpe] + $i * $offheap.sizeOf[$A]
        ${read(q"$naddr", A)}
      """
    }

  def update(index: Tree, value: Tree) =
    boundsChecked(index) { pre => i =>
      val naddr = fresh("addr")
      q"""
        val $naddr = $pre.$addr + $offheap.sizeOf[$SizeTpe] + $i * $offheap.sizeOf[$A]
        ${write(q"$naddr", A, value)}
      """
    }

  def foreach(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      q"""
        if ($pre.nonEmpty)
          ${iterate(A, pre, p => app(f, read(p, A)))}
      """
    }

  def iterate(T: Type, pre: Tree, f: Tree => Tree) = {
    val p = fresh("p")
    val len = fresh("len")
    val step = fresh("step")
    val bound = fresh("bound")
    q"""
      var $p: $AddrTpe = $pre.$addr
      val $len: $SizeTpe = ${read(q"$p", SizeTpe)}
      $p += $offheap.sizeOf[$SizeTpe]
      val $step: $SizeTpe = $offheap.sizeOf[$T]
      val $bound: $AddrTpe = $p + $len * $step
      while ($p < $bound) {
        ${f(q"$p")}
        $p += $step
      }
    """
  }

  def map[B: WeakTypeTag](f: Tree)(a: Tree) = {
    val B = wt[B]
    assertAllocatable(B)
    stabilized(c.prefix.tree) { pre =>
      stabilized(a) { alloc =>
        val narr = fresh("narr")
        val v    = fresh("v")
        val p    = fresh("p")
        val step = fresh("step")
        q"""
          val $narr = $ArrayModule.uninit[$B]($pre.length)($alloc)
          val $step = $offheap.sizeOf[$B]
          var $p    = $narr.$addr + $offheap.sizeOf[$SizeTpe]
          $pre.foreach { $v: $A =>
            ${write(q"$p", B, app(f, q"$v"))}
            $p += $step
          }
          $narr
        """
      }
    }
  }

  def uninit[T: WeakTypeTag](n: Tree)(a: Tree) = {
    val T = wt[T]
    assertAllocatable(T)
    stabilized(n) { len =>
      stabilized(a) { alloc =>
        val naddr = fresh("addr")
        val size = q"$offheap.sizeOf[$AddrTpe] + $len * $offheap.sizeOf[$T]"
        q"""
          if ($len < 0) throw new $IllegalArgumentExceptionClass
          else if ($len == 0) $ArrayModule.empty[$T]
          else {
            val $naddr = $alloc.allocate($size)
            ${write(q"$naddr", SizeTpe, len)}
            $ArrayModule.fromAddr[$T]($naddr)
          }
        """
      }
    }
  }

  def vararg[T: WeakTypeTag](values: Tree*)(a: Tree) = {
    val T = wt[T]
    assertAllocatable(T, s"Can't allocate offheap array of $T")
    if (values.isEmpty)
      q"$ArrayModule.empty[$T]"
    else stabilized(a) { alloc =>
      val arr    = fresh("arr")
      val naddr  = fresh("adr")
      val step   = fresh("step")
      val writes = values.zipWithIndex.map { case (v, i) =>
        write(q"$naddr + $i * $step", T, v)
      }
      q"""
        val $arr = $ArrayModule.uninit[$T](${values.length})($alloc)
        val $step = $offheap.sizeOf[$T]
        val $naddr = $arr.$addr + $offheap.sizeOf[$AddrTpe]
        ..$writes
        $arr
      """
    }
  }

  def fill[T: WeakTypeTag](n: Tree)(elem: Tree)(a: Tree) = {
    stabilized(n) { len =>
      stabilized(a) { alloc =>
        val T   = wt[T]
        val arr = fresh("arr")
        q"""
          val $arr = $ArrayModule.uninit[$T]($len)($alloc)
          ${iterate(T, q"$arr", p => write(p, T, elem))}
          $arr
        """
      }
    }
  }

  // TODO: implement me
  def copy[T: WeakTypeTag](from: Tree, fromIndex: Tree,
                           to: Tree, toIndex: Tree, size: Tree) = q"???"
}
