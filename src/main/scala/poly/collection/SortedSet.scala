package poly.collection

/**
  * Represents a set when iterated, its keys are sorted.
  * @since 0.1.0
  */
trait SortedSet[T] extends Set[T] {

  def orderOnKey = keys.order

  def equivOnKey = orderOnKey

  def keys: SortedIterable[T]

  override def elements = keys

}
