package poly.collection

import poly.collection.specgroup._

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

  /** Returns the minimum and the maximum element of this sorted set under the inherent order of this sorted set. */
  def minAndMax = (min, max)

  //TODO: subsetBetween, subsetUpTo, subsetFrom?

  override def createMap[V](f: T => V): KeySortedMap[T, V] = new SortedSetT.MapByFunc(self, f)

}

abstract class AbstractSortedSet[@sp(Int) T] extends SortedSet[T]

private[poly] object SortedSetT {

  class Filtered[T](self: SortedSet[T], f: T => Boolean) extends AbstractSortedSet[T] {
    def keyOrder = self.keyOrder
    def contains(x: T) = self.contains(x) && f(x)
    def keys = self.keys filter f
  }

  class MapByFunc[T, V](self: SortedSet[T], f: T => V) extends SetT.MapByFunc[T, V](self, f) with KeySortedMap[T, V] {
    override def keySet = self.keySet
  }
}