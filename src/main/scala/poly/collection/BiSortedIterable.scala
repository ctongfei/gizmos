package poly.collection

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiSortedIterable[T] extends BiIterable[T] with SortedIterable[T] { self =>

  override def reverse: BiSortedIterable[T] = new BiSortedIterable[T] {
    implicit def elementOrder = self.elementOrder.reverse
    def newReverseIterator = self.newIterator
    def newIterator = self.newReverseIterator
  }

}
