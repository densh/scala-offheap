package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Array(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions.{ ArrayClass => JArrayClass, ArrayModule => JArrayModule, _ }

  lazy val A = c.prefix.tree.tpe.baseType(ArrayClass).typeArgs.head

  def isEmpty = q"${c.prefix.tree}.$addr == 0L"

  def nonEmpty = q"${c.prefix.tree}.$addr != 0L"

  def size = stabilized(c.prefix.tree) { pre =>
    read(q"$pre.$addr", ArraySizeTpe)
  }

  def sizeOfArraySize = q"$offheap.sizeOf[$ArraySizeTpe]"

  def throwIllegalArgument(v: Tree) =
    q"throw new $IllegalArgumentExceptionClass($v.toString)"

  def throwOutOfBounds(idx: Tree) =
    q"throw new $IndexOutOfBoundsExceptionClass($idx.toString)"

  def boundsChecked(index: Tree)(ifOk: Tree => Tree => Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(index) { idx =>
        q"""
          if ($CheckedModule.BOUNDS)
            if ($idx < 0 || $idx >= ${read(q"$pre.$addr", ArraySizeTpe)})
              ${throwOutOfBounds(idx)}
          ${ifOk(pre)(idx)}
        """
      }
    }

  def apply(index: Tree) =
    boundsChecked(index) { pre => i =>
      read(q"$pre.$addr + $sizeOfArraySize + $i * $offheap.sizeOf[$A]", A)
    }

  def update(index: Tree, value: Tree) =
    boundsChecked(index) { pre => i =>
      write(q"$pre.$addr + $sizeOfArraySize + $i * $offheap.sizeOf[$A]", A, value)
    }

  def foreach(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      q"""
        if ($pre.nonEmpty)
          ${iterate(A, pre, _ => p => app(f, read(p, A)))}
      """
    }

  def iterate(T: Type, pre: Tree, f: Tree => Tree => Tree) = {
    val i = freshVar("i", IntTpe, q"0")
    val len = freshVal("len", ArraySizeTpe, read(q"$pre.$addr", ArraySizeTpe))
    val base = freshVal("base", AddrTpe, q"$pre.$addr + $sizeOfArraySize")
    q"""
      $len
      $base
      $i
      while (${i.symbol} < ${len.symbol}) {
        ..${f(q"${i.symbol}")(q"${base.symbol} + ${i.symbol} * ${sizeOf(T)}")}
        ${i.symbol} += 1
      }
    """
  }

  def map[B: WeakTypeTag](f: Tree)(a: Tree) = {
    val B = wt[B]
    assertAllocatable(B)
    stabilized(c.prefix.tree) { pre =>
      stabilized(a) { alloc =>
        val narr = freshVal("narr", appliedType(ArrayTpe, B),
                            q"$ArrayModule.uninit[$B]($pre.length)($alloc)")
        val base = freshVal("base", AddrTpe,
                            q"${narr.symbol}.$addr + $sizeOfArraySize")
        val body =
          iterate(A, pre, i => p => q"""
            ..${write(q"${base.symbol} + ${i.symbol} * ${sizeOf(B)}", B, app(f, read(p, A)))}
          """)
        q"""
          if ($pre.isEmpty) $ArrayModule.empty[$B]
          else {
            $narr
            $base
            ..$body
            ${narr.symbol}
          }
        """
      }
    }
  }

  def toArray =
    stabilized(c.prefix.tree) { pre =>
      val size = fresh("size")
      val jarr = fresh("jarr")
      val i    = fresh("i")
      q"""
        if ($pre.isEmpty) $JArrayModule.empty[$A]
        else {
          val $size = ${read(q"$pre.$addr", ArraySizeTpe)}
          val $jarr = new $JArrayClass[$A]($size)
          var $i    = 0
          while ($i < $size) {
            $jarr($i) = $pre($i)
            $i += 1
          }
          $jarr
        }
      """
    }

  def clone_(a: Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(a) { alloc =>
        val size = fresh("size")
        val narr = fresh("narr")
        q"""
          if ($pre.isEmpty) $ArrayModule.empty[$A]
          else {
            val $size = ${read(q"$pre.$addr", ArraySizeTpe)}
            val $narr = $ArrayModule.uninit[$A]($size)($alloc)
            $ArrayModule.copy($pre, 0, $narr, 0, $size)
            $narr
          }
        """
      }
    }

  def uninit[T: WeakTypeTag](n: Tree)(a: Tree) = {
    val T = wt[T]
    assertAllocatable(T)
    stabilized(n) { len =>
      stabilized(a) { alloc =>
        val size  = q"$sizeOfArraySize + $len * $offheap.sizeOf[$T]"
        val naddr = freshVal("addr", AddrTpe, q"$alloc.allocate($size)")
        q"""
          if ($len < 0) throw new $IllegalArgumentExceptionClass
          else if ($len == 0) $ArrayModule.empty[$T]
          else {
            ${naddr}
            ${write(q"${naddr.symbol}", ArraySizeTpe, len)}
            $ArrayModule.fromAddr[$T](${naddr.symbol})
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
        val $arr   = $ArrayModule.uninit[$T](${values.length})($alloc)
        val $step  = $offheap.sizeOf[$T]
        val $naddr = $arr.$addr + $sizeOfArraySize
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
          if ($len == 0) $ArrayModule.empty[$T]
          else {
            val $arr = $ArrayModule.uninit[$T]($len)($alloc)
            ${iterate(T, q"$arr", _ => p => write(p, T, elem))}
            $arr
          }
        """
      }
    }
  }

  def copy[T: WeakTypeTag](from: Tree, fromIndex: Tree,
                           to: Tree, toIndex: Tree, size: Tree) = {
    def checks(arr: Tree, indexes: Tree*) = {
      val size = fresh("size")
      val checks = indexes.map { idx =>
        q"if ($idx < 0 || $idx >= $size) ${throwOutOfBounds(idx)}"
      }
      q"""
        val $size = ${read(q"$arr.$addr", ArraySizeTpe)}
        ..$checks
      """
    }
    stabilized(from) { fromArr =>
      stabilized(fromIndex) { fromIdx =>
        stabilized(to) { toArr =>
          stabilized(toIndex) { toIdx =>
            stabilized(size) { count =>
              val fromChecks = checks(fromArr, fromIdx, q"$fromIdx + $count - 1")
              val toChecks   = checks(toArr, toIdx, q"$toIdx + $count - 1")
              val stride     = q"$offheap.sizeOf[${wt[T]}]"
              val fromAddr   = q"$fromArr.$addr + $sizeOfArraySize + $fromIdx * $stride"
              val toAddr     = q"$toArr.$addr + $sizeOfArraySize + $toIdx * $stride"
              val sizeBytes  = q"$count * $stride"
              q"""
                if ($count <= 0)
                  ${throwIllegalArgument(count)}
                if ($fromArr.isEmpty)
                  ${throwIllegalArgument(fromArr)}
                if ($toArr.isEmpty)
                  ${throwIllegalArgument(toArr)}
                if ($CheckedModule.BOUNDS) {
                  ..$fromChecks
                  ..$toChecks
                }
                $MemoryModule.copy($fromAddr, $toAddr, $sizeBytes)
              """
            }
          }
        }
      }
    }
  }

  def fromArray[T: WeakTypeTag](arr: Tree)(a: Tree) =
    stabilized(arr) { jarr =>
      stabilized(a) { alloc =>
        val arr = fresh("arr")
        val i   = fresh("i")
        q"""
          if ($jarr.isEmpty) $ArrayModule.empty[${wt[T]}]
          else {
            val $arr = $ArrayModule.uninit[${wt[T]}]($jarr.length)($alloc)
            var $i = 0
            while ($i < $jarr.length) {
              $arr($i) = $jarr($i)
              $i += 1
            }
            $arr
          }
        """
      }
    }

}
