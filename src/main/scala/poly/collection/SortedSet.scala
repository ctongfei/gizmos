package poly.collection

import poly.algebra._

/**
 * Represents a set whose keys are sorted when being iterated.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait SortedSet[T] extends Set[T] { self ⇒

  /** Returns the order on keys. */
  def orderOnKeys: Order[T]

  def eqOnKeys = orderOnKeys

  def keys: SortedIterable[T]

  override def keySet = this

  override def elements = keys

  /** Returns the minimum element of this sorted set under the inherent order of this sorted set. */
  def min = keys.head

  /** Returns the maximum element of this sorted set under the inherent order of this sorted set. */
  def max = keys.last

  def subsetBetween(lowerBound: T, upperBound: T): SortedSet[T] = ???

  def subsetFrom(lowerBound: T): SortedSet[T] = ???

  def subsetUpTo(upperBound: T): SortedSet[T] = ???

  override def createMapBy[V](f: T ⇒ V): SortedMap[T, V] = new SortedMap[T, V] {
    def apply(k: T) = f(k)
    def ?(k: T) = if (self contains k) Some(f(k)) else None
    def orderOnKeys = self.orderOnKeys
    def pairs = self.keys.map(k ⇒ k → f(k)).asIfSorted(eqOnKeys contramap first[T, V])
    override def size = self.size
    def containsKey(x: T) = self.contains(x)
  }

}

abstract class AbstractSortedSet[T] extends AbstractSet[T] with SortedSet[T]
