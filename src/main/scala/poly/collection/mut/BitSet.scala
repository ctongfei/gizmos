package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.impl.specialized._
import poly.macroutil._

/**
 * Represents a bitset, which is internally represented as an array of `Long`s.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class BitSet private(private final var data: LongResizableArray)
  extends AbstractSet[Int] with SortedSet[Int] with KeyMutableSet[Int] { self =>

  import BitSet._

  implicit def keyOrder = std.IntStructure

  def addInplace(x: Int) = {
    val idx = x >> LongBits
    data.ensureCapacity(idx + 1)
    data(idx) |= (1l << x)
  }

  def removeInplace(x: Int) = {
    val idx = x >> LongBits
    if (idx < data.capacity) {
      data(idx) &= ~(1l << x)
    }
  }

  def contains(x: Int) = {
    val idx = x >> LongBits
    if (idx < data.capacity) (data(idx) & (1l << x)) != 0l
    else false
  }

  def keys = Iterable.ofIterator {
    new AbstractIterator[Int] {
      private[this] var idx = 0
      private[this] var bitMask = 1l
      private[this] var k = 0
      private[this] var curr = -1
      def advance(): Boolean = {
        while (idx < data.capacity) {
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

  override def foreach[U](f: Int => U) = {
    FastLoop.ascending(0, data.capacity, 1) { i =>
      var w = data(i)
      var j = i * LongSize
      while (w != 0l) {
        if ((w & 1l) == 1l) f(j)
        w >>>= 1
        j += 1
      }
    }
  }

  def clear() = data.fillInplace(0l)

  //TODO: eager version of union, intersect, setDiff, symmetricDiff

}

object BitSet {

  private[poly] final val LongBits = 6 // 2^6 = 64
  private[poly] final val LongSize = 64

  def apply() = new BitSet(new LongResizableArray(Settings.ArrayInitialSize))

  implicit def newBuilder: Builder[Int, BitSet] = new Builder[Int, BitSet] {
    private[this] val bs = BitSet()
    def addInplace(x: Int) = bs addInplace x
    def result = bs
  }

}