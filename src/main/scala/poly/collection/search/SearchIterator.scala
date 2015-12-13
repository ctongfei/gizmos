package poly.collection.search

import poly.collection._

/**
 * Represents an iterator in a search algorithm.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait SearchIterator[N, S] extends Iterator[S] {

  /** Returns the current node of this search iterator. */
  def currentNode: N

  /** Returns the current state of this search iterator. */
  def current: S

}
