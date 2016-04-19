package poly.collection

import poly.algebra._

/**
  * Represents a set whose keys are sorted when being iterated.
  * @since 0.1.0
  */
trait SortedSet[T] extends Set[T] {

  /** Returns the order on keys. */
  def orderOnKey: WeakOrder[T]

  def equivOnKey = orderOnKey

  def keys: SortedIterable[T]

  override def elements = keys

}
