package poly.collection.search

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class AStarEnumerator[S, C](start: S)(implicit ss: StateSpaceWithHeuristic[S, C]) extends Enumerator[S] {
  /**
   * Advances the enumerator to the next element.
   * @return Whether the enumerator successfully advanced to the next element.
   */
  def advance() = ???

  /** Returns the current element of this enumeration. */
  def current = ???
}
