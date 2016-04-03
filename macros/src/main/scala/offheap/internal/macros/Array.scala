package scala.offheap
package internal
package macros

import scala.reflect.macros.blackbox

trait ArrayCommon extends Common {
  val c: blackbox.Context
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions.{ ArrayClass => JArrayClass, ArrayModule => JArrayModule, _ }

  def isEmbed: Boolean
  def MyArrayClass  = if (isEmbed) EmbedArrayClass else ArrayClass
  def MyArrayTpe    = MyArrayClass.asType.toType
  def MyArrayModule = if (isEmbed) EmbedArrayModule else ArrayModule

  def throwIllegalArgument(v: Tree) =
    q"throw new $IllegalArgumentExceptionClass($v.toString)"

  def throwOutOfBounds(idx: Tree) =
    q"throw new $IndexOutOfBoundsExceptionClass($idx.toString)"

  def strideOf(T: Type): Long = strideOf(T, isEmbed)

  def sizeOfHeader =
    q"$offheap.sizeOf[$LongTpe]"

  def alignment =
    q"$offheap.alignmentOf[$LongTpe]"

  def readSize(pre: Tree) =
    read(q"$pre.addr", ArraySizeTpe)

  def writeSize(pre: Tree, value: Tree) =
    write(q"$pre.addr", ArraySizeTpe, value)

  def readElem(pre: Tree, T: Type, idx: Tree) = {
    val elemAddr = q"$pre.addr + $sizeOfHeader + $idx * ${strideOf(T)}"
    if (isEmbed) readEmbed(elemAddr, T)
    else read(elemAddr, T)
  }

  def writeElem(pre: Tree, T: Type, idx: Tree, value: Tree) = {
    val elemAddr = q"$pre.addr + $sizeOfHeader + $idx * ${strideOf(T)}"
    if (isEmbed) writeEmbed(elemAddr, T, value)
    else write(elemAddr, T, value)
  }

  def iterate(pre: Tree, T: Type, f: Tree => Tree) = {
    val i = freshVar("i", IntTpe, q"0")
    val len = freshVal("len", ArraySizeTpe, read(q"$pre.addr", ArraySizeTpe))
    val base = freshVal("base", AddrTpe, q"$pre.addr + $sizeOfHeader")
    q"""
      $len
      $base
      $i
      while (${i.symbol} < ${len.symbol}) {
        ..${f(q"${i.symbol}")}
        ${i.symbol} += 1
      }
    """
  }
}

trait ArrayApiCommon extends ArrayCommon {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions.{ ArrayClass => JArrayClass, ArrayModule => JArrayModule, _ }

  lazy val A = {
    val res = c.prefix.tree.tpe.baseType(MyArrayClass).typeArgs.head
    assertAllocatable(res, s"can't use an array of non-allocatable $res elements")
    res
  }

  def isEmpty = q"${c.prefix.tree}.addr == 0L"

  def nonEmpty = q"${c.prefix.tree}.addr != 0L"

  def size = stabilized(c.prefix.tree) { pre =>
    q"""
      if ($pre.isEmpty) 0
      else ${readSize(pre)}
    """
  }

  def boundsChecked(index: Tree)(ifOk: Tree => Tree => Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(index) { idx =>
        q"""
          if ($CheckedModule.BOUNDS) {
            if ($pre.isEmpty || $idx < 0 || $idx >= ${readSize(pre)})
              ${throwOutOfBounds(idx)}
          }
          ${ifOk(pre)(idx)}
        """
      }
    }

  def apply(index: Tree) =
    boundsChecked(index)(pre => idx => readElem(pre, A, idx))

  def update(index: Tree, value: Tree) =
    boundsChecked(index)(pre => idx => writeElem(pre, A, idx, value))

  def foreach(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      q"""
        if ($pre.nonEmpty)
          ${iterate(pre, A, idx => app(f, readElem(pre, A, idx)))}
      """
    }

  def map[B: WeakTypeTag](f: Tree)(a: Tree) = {
    val B = wt[B]
    assertAllocatable(B)
    stabilized(c.prefix.tree) { pre =>
      stabilized(a) { alloc =>
        val narr = freshVal("narr", appliedType(MyArrayTpe, B),
                            q"$MyArrayModule.uninit[$B]($pre.length)($alloc)")
        val base = freshVal("base", AddrTpe,
                            q"${narr.symbol}.addr + $sizeOfHeader")
        val body =
          iterate(pre, A, idx =>
            writeElem(q"${narr.symbol}", B, idx, app(f, readElem(pre, A, idx))))
        q"""
          if ($pre.isEmpty) $MyArrayModule.empty[$B]
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

  def transform(f: Tree) = {
    stabilized(c.prefix.tree) { pre =>
      val body = iterate(pre, A, idx => writeElem(q"${pre.symbol}", A, idx, app(f, readElem(pre, A, idx))))
      q"""
        if ($pre.nonEmpty) {
          ..$body
        }
        ${pre.symbol}
      """
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
          val $size = ${readSize(pre)}
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
          if ($pre.isEmpty) $MyArrayModule.empty[$A]
          else {
            val $size = ${readSize(pre)}
            val $narr = $MyArrayModule.uninit[$A]($size)($alloc)
            $MyArrayModule.copy($pre, 0, $narr, 0, $size)
            $narr
          }
        """
      }
    }

  def filter(f: Tree)(a: Tree) = {
    stabilized(c.prefix.tree) { pre =>
      stabilized(a) { alloc =>
        val newArray = freshVal("narr", appliedType(MyArrayTpe, A),
          q"$MyArrayModule.uninit[$A]($pre.length)($alloc)")

        val sourceLength = freshVal("len", ArraySizeTpe, read(q"$pre.addr", ArraySizeTpe))

        val sourceIndex = freshVar("i", IntTpe, q"0")
        val newArrayIndex = freshVar("j", IntTpe, q"0")

        val finalSize =
          freshVal("finalSize", LongTpe,
                   q"$sizeOfHeader + ${strideOf(A)} * ${newArrayIndex.symbol}")
        val sourceSize =
          freshVal("sourceSize", LongTpe,
                   q"${strideOf(A)} * ${sourceLength.symbol}")
        val finalAddress =
          freshVal("finalAddress", AddrTpe,
                   q"""$alloc.reallocate(${newArray.symbol}.addr, ${sourceSize.symbol}.toInt,
                                         ${finalSize.symbol}.toInt, $alignment)""")

        q"""
          if ($pre.isEmpty) $MyArrayModule.empty[$A]
          else {
            $sourceLength

            $newArray

            $sourceIndex
            $newArrayIndex
            while (${sourceIndex.symbol} < ${sourceLength.symbol}) {
              if (${app(f, readElem(pre, A, q"${sourceIndex.symbol}"))}) {
                ${writeElem(q"${newArray.symbol}", A, q"${newArrayIndex.symbol}", readElem(pre, A, q"${sourceIndex.symbol}"))}
                ${newArrayIndex.symbol} += 1
              }

              ${sourceIndex.symbol} += 1
            }

            if (${newArrayIndex.symbol} > 0) {
              $finalSize
              $sourceSize
              $finalAddress
              ${write(q"${finalAddress.symbol}", ArraySizeTpe, q"${newArrayIndex.symbol}")}
              $MyArrayModule.fromAddr[$A](${finalAddress.symbol})
            } else {
              $alloc.free(${newArray.symbol}.addr)
              $MyArrayModule.empty[$A]
            }
          }
        """
      }
    }
  }

  def foldLeft[B: WeakTypeTag](z: Tree, op: Tree) = {
    stabilized(c.prefix.tree) { pre =>
      stabilized(z) { z =>
        val acc = freshVar("acc", wt[B], z)
        val f = { idx: Tree => q"${acc.symbol} = ${app(op, q"${acc.symbol}", readElem(pre, A, idx))}" }
        q"""
          $acc
          if ($pre.nonEmpty) ${iterate(pre, A, f)}
          ${acc.symbol}
        """
      }
    }
  }

  def foldRight[B: WeakTypeTag](z: Tree, op: Tree) = {
    stabilized(c.prefix.tree) { pre =>
      stabilized(z) { z =>
        val acc = freshVar("acc", wt[B], z)
        val length = freshVal("len", ArraySizeTpe, read(q"$pre.addr", ArraySizeTpe))
        val index = freshVar("i", IntTpe, q"${length.symbol} - 1")
        q"""
          $acc
          if ($pre.nonEmpty) {
            $length
            $index
            while (${index.symbol} >= 0) {
              ${acc.symbol} = ${app(op, readElem(pre, A, q"${index.symbol}"), q"${acc.symbol}")}
              ${index.symbol} -= 1
            }
          }
          ${acc.symbol}
        """
      }
    }
  }

  def forall(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val sourceIndex = freshVar("i", IntTpe, q"0")
      val sourceLength = freshVal("len", ArraySizeTpe, read(q"$pre.addr", ArraySizeTpe))
      val result = freshVar("result", BooleanTpe, q"true")
      q"""
        if ($pre.isEmpty) true
        else {
          $sourceIndex
          $sourceLength
          $result
          while (${result.symbol} && ${sourceIndex.symbol} < ${sourceLength.symbol}) {
            ${result.symbol} = ${app(f, readElem(pre, A, q"${sourceIndex.symbol}"))}
            ${sourceIndex.symbol } += 1
          }

          ${result.symbol}
        }
      """
    }

  def exists(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val sourceIndex = freshVar("i", IntTpe, q"0")
      val sourceLength = freshVal("len", ArraySizeTpe, read(q"$pre.addr", ArraySizeTpe))
      val result = freshVar("result", BooleanTpe, q"false")
      q"""
        if ($pre.isEmpty) false
        else {
          $sourceIndex
          $sourceLength
          $result
          while (!${result.symbol} && ${sourceIndex.symbol} < ${sourceLength.symbol}) {
            ${result.symbol} = ${app(f, readElem(pre, A, q"${sourceIndex.symbol}"))}
            ${sourceIndex.symbol } += 1
          }

          ${result.symbol}
        }
      """
    }

  def sameElements(other: Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(other) { oth =>
        val sourceIndex = freshVar("i", IntTpe, q"0")
        val sourceLength = freshVal("len", ArraySizeTpe, read(q"$pre.addr", ArraySizeTpe))
        val otherLength = freshVal("othLen", ArraySizeTpe, read(q"$oth.addr", ArraySizeTpe))
        val result = freshVar("result", BooleanTpe, q"true")
        q"""
          if ($pre.addr == $oth.addr) true
          else if ($pre.isEmpty || $oth.isEmpty) false
          else {
            $sourceLength
            $otherLength

            if (${sourceLength.symbol} != ${otherLength.symbol}) false
            else {
              $sourceIndex
              $result
              while (${result.symbol} && ${sourceIndex.symbol} < ${sourceLength.symbol}) {
                ${result.symbol} =
                  ${readElem(pre, A, q"${sourceIndex.symbol}")} == ${readElem(oth, A, q"${sourceIndex.symbol}")}
                ${sourceIndex.symbol} += 1
              }

              ${result.symbol}
            }
          }
          """
      }
    }

  def startsWith(that: Tree): Tree = startsWithOffset(that, q"0")

  def startsWithOffset(that: Tree, offset: Tree): Tree = {
    stabilized(c.prefix.tree) { pre =>
      stabilized(that) { that =>
        stabilized(offset) { offset =>
          val index = freshVar("i", IntTpe, q"0")
          val sourceLength = freshVal("srcLen", ArraySizeTpe, readSize(pre))
          val thatLength = freshVal("thatLen", ArraySizeTpe, readSize(that))
          val result = freshVar("result", BooleanTpe, q"true")
          q"""
            if ($offset < 0) ${throwOutOfBounds(offset)}

            if ($pre.addr == $that.addr) true
            else if ($pre.isEmpty) false
            else if ($that.isEmpty) true
            else {
              $sourceLength
              $thatLength
              if ($offset + ${thatLength.symbol} > ${sourceLength.symbol}) false
              else {
                $index
                $result
                while (${result.symbol} && ${index.symbol} < ${thatLength.symbol}) {
                  ${result.symbol} =
                    ${readElem(pre, A, q"${index.symbol} + $offset")} == ${readElem(that, A, q"${index.symbol}")}
                  ${index.symbol} += 1
                }
                ${result.symbol}
              }
            }
          """
        }
      }
    }
  }

  def endsWith(that: Tree) = {
    stabilized(c.prefix.tree) { pre =>
      stabilized(that) { that =>
        val index = freshVar("i", IntTpe, q"0")
        val sourceLength = freshVal("srcLen", ArraySizeTpe, readSize(pre))
        val thatLength = freshVal("thatLen", ArraySizeTpe, readSize(that))
        val offset = freshVal("offset", IntTpe, q"${sourceLength.symbol} - ${thatLength.symbol}")
        val result = freshVar("result", BooleanTpe, q"true")
        q"""
          if ($pre.addr == $that.addr) true
          else if ($pre.isEmpty) false
          else if ($that.isEmpty) true
          else {
            $sourceLength
            $thatLength
            $offset
            if (${offset.symbol} < 0 || ${offset.symbol} + ${thatLength.symbol} > ${sourceLength.symbol}) false
            else {
              $index
              $result
              while (${result.symbol} && ${index.symbol} < ${thatLength.symbol}) {
                ${result.symbol} =
                  ${readElem(pre, A, q"${index.symbol} + ${offset.symbol}")} == ${readElem(that, A, q"${index.symbol}")}
                ${index.symbol} += 1
              }
              ${result.symbol}
            }
          }
        """
      }
    }
  }
}

trait ArrayModuleCommon extends ArrayCommon {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions.{ ArrayClass => JArrayClass, ArrayModule => JArrayModule, _ }

  def uninit[T: WeakTypeTag](n: Tree)(a: Tree) = {
    val T = wt[T]
    assertAllocatable(T)
    stabilized(n) { len =>
      stabilized(a) { alloc =>
        val size  = q"$sizeOfHeader + $len * ${strideOf(T)}"
        val naddr = freshVal("addr", AddrTpe, q"$alloc.allocate($size, $alignment)")
        q"""
          if ($len < 0) throw new $IllegalArgumentExceptionClass
          else if ($len == 0) $MyArrayModule.empty[$T]
          else {
            ${naddr}
            ${write(q"${naddr.symbol}", ArraySizeTpe, len)}
            $MyArrayModule.fromAddr[$T](${naddr.symbol})
          }
        """
      }
    }
  }

  def vararg[T: WeakTypeTag](values: Tree*)(a: Tree) = {
    val T = wt[T]
    assertAllocatable(T, s"Can't allocate offheap array of $T")
    if (values.isEmpty)
      q"$MyArrayModule.empty[$T]"
    else stabilized(a) { alloc =>
      val arr    = fresh("arr")
      val naddr  = fresh("addr")
      val stride = strideOf(T)
      val writes = values.zipWithIndex.map { case (v, i) =>
        writeElem(q"$arr", T, q"$i", v)
      }.toList
      q"""
        val $arr   = $MyArrayModule.uninit[$T](${values.length})($alloc)
        val $naddr = $arr.addr + $sizeOfHeader
        ..${flatten(writes)}
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
          if ($len == 0) $MyArrayModule.empty[$T]
          else {
            val $arr = $MyArrayModule.uninit[$T]($len)($alloc)
            ..${iterate(q"$arr", T, idx => writeElem(q"$arr", T, idx, elem))}
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
        val $size = ${readSize(arr)}
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
              val stride     = strideOf(wt[T])
              val fromAddr   = q"$fromArr.addr + $sizeOfHeader + $fromIdx * $stride"
              val toAddr     = q"$toArr.addr + $sizeOfHeader + $toIdx * $stride"
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
          if ($jarr.isEmpty) $MyArrayModule.empty[${wt[T]}]
          else {
            val $arr = $MyArrayModule.uninit[${wt[T]}]($jarr.length)($alloc)
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

trait NotEmbed { def isEmbed = false }
trait IsEmbed { def isEmbed = true }

class ArrayApi(val c: blackbox.Context) extends ArrayApiCommon with NotEmbed
class ArrayModule(val c: blackbox.Context) extends ArrayModuleCommon with NotEmbed

class EmbedArrayApi(val c: blackbox.Context) extends ArrayApiCommon with IsEmbed
class EmbedArrayModule(val c: blackbox.Context) extends ArrayModuleCommon with IsEmbed
