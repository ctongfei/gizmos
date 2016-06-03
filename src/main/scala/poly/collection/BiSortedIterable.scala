package poly.collection

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiSortedIterable[T] extends BiIterable[T] with SortedIterable[T] { self =>

  override def reverse: BiSortedIterable[T] = new BiSortedIterable[T] {
    implicit def orderOnElements = self.orderOnElements.reverse
    def newReverseIterator = self.newIterator
    def newIterator = self.newReverseIterator
  }

}
