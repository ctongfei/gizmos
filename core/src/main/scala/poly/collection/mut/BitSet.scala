package poly.collection.mut

import poly.collection._
import poly.collection.evidence._
import poly.collection.factory._
import poly.collection.impl._

/**
 * Represents a bitset, which is internally represented as an array of `Long`s.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class BitSet private(private final val data: BitResizableArray)
  extends AbstractSortedSet[Int] with KeyMutableSet[Int] { self =>

  //TODO: min and max could be optimized

  implicit def keyOrder = Order[Int]

  def add_!(x: Int) = data(x) = true

  def remove_!(x: Int) = data(x) = false

  def contains(x: Int) = data(x)

  def keys = data.trueKeys

  override def foreach[U](f: Int => U) = data.foreachTrueKey(f)

  def clear_!() = data.clear()

  /** Returns the union of two bitsets as a bitset. */
  def unionE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length max b.length)(i => a(i) | b(i))
    new BitSet(new BitResizableArray(c))
  }

  /** Returns the intersection of two bitsets as a bitset. */
  def intersectE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length min b.length)(i => a(i) & b(i))
    new BitSet(new BitResizableArray(c))
  }

  /** Returns the set difference of two bitsets as a bitset. */
  def diffE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length)(i => a(i) & ~b(i))
    new BitSet(new BitResizableArray(c))
  }

  /** Returns the symmetric difference of two bitsets as a bitset. */
  def symmetricDiffE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length max b.length)(i => a(i) ^ b(i))
    new BitSet(new BitResizableArray(c))
  }

}

object BitSet extends SetFactory[({type λ[α] = BitSet})#λ, IsInt] { // SI-2712: Partial type lambda unification

  implicit def newSetBuilder[T: IsInt]: RemovableBuilder[T, BitSet] = new RemovableBuilder[Int, BitSet] {
    private[this] val ba = new BitResizableArray(new Array[Long](Settings.ArrayInitialSize))
    override def sizeHint(n: Int) = ba.ensureCapacity(n >> BitResizableArray.LongBits)
    def add(x: Int) = ba(x) = true
    def remove(x: Int) = ba(x) = false
    def result = new BitSet(ba)
  }.asInstanceOf[RemovableBuilder[T, BitSet]] // typecast safe, T =:= Int is known

}
