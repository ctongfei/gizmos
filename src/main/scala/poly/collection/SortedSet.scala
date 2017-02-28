package poly.collection

import poly.algebra._
import poly.algebra.specgroup._

/**
 * Represents a set whose keys are sorted when being iterated.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait SortedSet[@sp(Int) T] extends Set[T] { self =>

  /** Returns the order on keys. */
  def keyOrder: Order[T]

  def keyEq = keyOrder

  def keys: SortedIterable[T]

  override def keySet = this

  override def elements = keys

  override def filterKeys(f: T => Boolean): SortedSet[T] = new SortedSetT.Filtered(self, f)

  override def filter(f: T => Boolean) = filterKeys(f)

  /** Returns the minimum element of this sorted set under the inherent order of this sorted set. */
  def min = keys.head

  /** Returns the maximum element of this sorted set under the inherent order of this sorted set. */
  def max = keys.last

  //TODO: subsetBetween, subsetUpTo, subsetFrom?

  override def createMap[V](f: T => V): KeySortedMap[T, V] = new AbstractKeySortedMap[T, V] {
    def apply(k: T) = f(k)
    def ?(k: T) = if (self contains k) Some(f(k)) else None
    override def size = self.size
    def keySet = self.keySet
  }

}

abstract class AbstractSortedSet[@sp(Int) T] extends AbstractSet[T] with SortedSet[T]

private[poly] object SortedSetT {

  class Filtered[T](self: SortedSet[T], f: T => Boolean) extends AbstractSortedSet[T] {
    def keyOrder = self.keyOrder
    def contains(x: T) = self.contains(x) && f(x)
    def keys = self.keys filter f
  }

}