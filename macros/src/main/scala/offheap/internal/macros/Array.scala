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
    read(q"$pre.$addr", ArraySizeTpe)
  }

  def sizeOfArraySize = q"$offheap.sizeOf[$ArraySizeTpe]"

  def boundsChecked(index: Tree)(ifOk: Tree => Tree => Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(index) { idx =>
        q"""
          if ($CheckedModule.BOUNDS)
            if ($idx < 0 || $idx >= ${read(q"$pre.$addr", ArraySizeTpe)})
              throw new _root_.java.lang.IndexOutOfBoundsException($idx.toString)
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

  def foreach(f: Tree) = debug("foreach") {
    stabilized(c.prefix.tree) { pre =>
      q"""
        if ($pre.nonEmpty)
          ${iterate(A, pre, _ => p => app(f, read(p, A)))}
      """
    }
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

  def map[B: WeakTypeTag](f: Tree)(a: Tree) = debug("map") {
    val B = wt[B]
    assertAllocatable(B)
    stabilized(c.prefix.tree) { pre =>
      stabilized(a) { alloc =>
        val narr   = freshVal("narr", appliedType(ArrayTpe, B),
                       q"$ArrayModule.uninit[$B]($pre.length)($alloc)")
        val base   = freshVal("base", AddrTpe,
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

  def uninit[T: WeakTypeTag](n: Tree)(a: Tree) = {
    val T = wt[T]
    assertAllocatable(T)
    stabilized(n) { len =>
      stabilized(a) { alloc =>
        val size = q"$sizeOfArraySize + $len * $offheap.sizeOf[$T]"
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
          val $arr = $ArrayModule.uninit[$T]($len)($alloc)
          ${iterate(T, q"$arr", _ => p => write(p, T, elem))}
          $arr
        """
      }
    }
  }

  // TODO: implement me
  def copy[T: WeakTypeTag](from: Tree, fromIndex: Tree,
                           to: Tree, toIndex: Tree, size: Tree) = q"???"
}
