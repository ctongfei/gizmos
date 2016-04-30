package poly.collection

/**
 * Represents a sorted set in which the keys can be iterated both ascendingly and descendingly.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiSortedSet[T] extends SortedSet[T] {

  def keys: BiSortedIterable[T]

}

