package poly.collection

import poly.algebra._

/**
  * Represents a set whose keys are sorted when being iterated.
  * @since 0.1.0
  */
trait SortedSet[T] extends Set[T] {

  /** Returns the order on keys. */
  def orderOnKeys: WeakOrder[T]

  def equivOnKeys = orderOnKeys

  def keys: SortedIterable[T]

  override def elements = keys

  /** Returns the minimum element of this sorted set under the inherent order of this sorted set. */
  def min = keys.head

  /** Returns the maximum element of this sorted set under the inherent order of this sorted set. */
  def max = keys.last

}
