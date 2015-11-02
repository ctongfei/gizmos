package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait SortedSet[T] extends Set[T] {

  def orderOnKey = elements.orderOnValue

  def equivOnKey = orderOnKey

  def elements: SortedIterable[T]


}
