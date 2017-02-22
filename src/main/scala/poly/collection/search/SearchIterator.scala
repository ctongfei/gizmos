package poly.collection.search

import poly.collection._

/**
 * Represents an iterator in a search algorithm.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait SearchIterator[S, N] extends Iterator[S] {

  /** Returns the current state of this search iterator. */
  def current: S

  /** Returns the current node of this search iterator. */
  def currentNode: N

}
