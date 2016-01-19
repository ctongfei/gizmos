package poly.collection

/**
  * Represents a set whose keys are sorted when being iterated.
  * @since 0.1.0
  */
trait SortedSet[T] extends Set[T] {

  /** Returns the order on keys. */
  def orderOnKey = keys.order

  def equivOnKey = orderOnKey

  def keys: SortedIterable[T]

  override def elements = keys

}
