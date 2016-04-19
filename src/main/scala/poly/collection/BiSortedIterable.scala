package poly.collection

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiSortedIterable[T] extends BiIterable[T] with SortedIterable[T] { self =>

  def max = self.last

}
