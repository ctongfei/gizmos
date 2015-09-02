package poly.collection.search

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait SearchIterator[+S] extends Iterator[S] {

  def currentNode: SearchNode[S]

  def current = currentNode.state

}
