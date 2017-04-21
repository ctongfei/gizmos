package poly.collection.impl

import poly.algebra.syntax._
import poly.collection._
import poly.macroutil._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
class BitResizableArray(private[this] var data: Array[Long]) { self =>

  import BitResizableArray._

  private[this] var cap: Int = data.size

  def longArray = data

  def ensureCapacity(minCapacity: Int): Unit = {
    if (cap < minCapacity) {
      val newCapacity = nextPowerOfTwo(minCapacity)
      val newData = Array.ofDim[Long](newCapacity)
      System.arraycopy(data, 0, newData, 0, cap)
      data = newData
      cap = newCapacity
    }
  }

  def apply(x: Int) = {
    val idx = x >> LongBits
    if (idx < cap) (data(idx) & (1l << x)) != 0l
    else false
  }

  def update(x: Int, v: Boolean) = {
    val idx = x >> LongBits
    if (v) {
      ensureCapacity(idx + 1)
      data(idx) |= (1l << x)
    }
    else if (idx < cap) {
      data(idx) &= ~(1l << x)
    }
    else { /* do nothing */ }
  }

  //TODO: bidirectional iterator
  def trueKeys = Iterable.ofIterator {
    new AbstractIterator[Int] {
      private[this] var idx = 0
      private[this] var bitMask = 1l
      private[this] var k = 0
      private[this] var curr = -1
      def advance(): Boolean = {
        while (idx < cap) {
          val word = data(idx)
          while (k < LongSize) {
            if ((word & bitMask) != 0l) {
              curr = idx * LongSize + k
              k += 1
              bitMask <<= 1
              return true
            }
            k += 1
            bitMask <<= 1
          }
          idx += 1
          k = 0
        }
        false
      }
      def current = curr
    }
  }.asIfSorted

  def foreachTrueKey[U](f: Int => U) = {
    FastLoop.ascending(0, cap, 1) { i =>
      var w = data(i)
      var j = i * LongSize
      while (w != 0l) {
        if ((w & 1l) == 1l) f(j)
        w >>>= 1
        j += 1
      }
    }
  }

  def clear() = FastLoop.ascending(0, cap, 1) { i =>
    data(i) = 0l
  }

}

object BitResizableArray {

  final val LongBits = 6 // 2 ^ 6 = 64
  final val LongSize = 64

}