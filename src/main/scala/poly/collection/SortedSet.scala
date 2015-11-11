package poly.collection

/**
  * Represents a set when iterated, its keys are sorted.
  * @since 0.1.0
  */
trait SortedSet[T] extends Set[T] {

  def orderOnKey = elements.orderOnValue

  def equivOnKey = orderOnKey

  def elements: SortedIterable[T]


}
