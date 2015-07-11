package poly.collection.search

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class AStarEnumerator[S, C](start: S)(implicit ss: StateSpaceWithHeuristic[S, C]) extends Enumerator[S] {

  def advance() = ???

  def current = ???
}
