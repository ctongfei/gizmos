package poly.collection.search

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait SearchIterator[S, N] extends Iterator[S] {

  def currentNode: N

  def currentState: S

  def current = currentState

}
