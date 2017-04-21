package poly.collection

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BidiSortedIterable[T] extends BidiIterable[T] with SortedIterable[T] { self =>

  override def reverse: BidiSortedIterable[T] = new BidiSortedIterable[T] {
    implicit def elementOrder = self.elementOrder.reverse
    def newReverseIterator = self.newIterator
    def newIterator = self.newReverseIterator
  }

}
