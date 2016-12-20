package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.factory._
import poly.collection.impl._

/**
 * Represents a bitset, which is internally represented as an array of `Long`s.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class BitSet private(private final val data: BitResizableArray)
  extends AbstractSortedSet[Int] with KeyMutableSet[Int] { self =>

  implicit def keyOrder = std.IntStructure

  def add_!(x: Int) = data(x) = true

  def remove_!(x: Int) = data(x) = false

  def contains(x: Int) = data(x)

  def keys = data.trueKeys

  override def foreach[U](f: Int => U) = data.foreachTrueKey(f)

  def clear_!() = data.clear()

  def unionE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length max b.length)(i => a(i) | b(i))
    new BitSet(new BitResizableArray(c))
  }

  def intersectE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length min b.length)(i => a(i) & b(i))
    new BitSet(new BitResizableArray(c))
  }

  def diffE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length)(i => a(i) & ~b(i))
    new BitSet(new BitResizableArray(c))
  }

  def symmetricDiffE(that: BitSet) = {
    val a = self.data.longArray
    val b = that.data.longArray
    val c = Array.tabulate(a.length max b.length)(i => a(i) ^ b(i))
    new BitSet(new BitResizableArray(c))
  }

}

object BitSet extends BuilderFactory1Ev1[({type λ[α] = BitSet})#λ, IsInt] {

  implicit def newBuilder[T: IsInt]: Builder[T, BitSet] = new Builder[Int, BitSet] {
    private[this] val ba = new BitResizableArray(new Array[Long](Settings.ArrayInitialSize))
    override def sizeHint(n: Int) = ba.ensureCapacity(n >> BitResizableArray.LongBits)
    def add(x: Int) = ba(x) = true
    def result = new BitSet(ba)
  }.asInstanceOf[Builder[T, BitSet]] // typecast safe, T =:= Int is known

}
