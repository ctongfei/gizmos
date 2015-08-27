package poly.collection.search

import poly.collection._

/**
 * Defines a space of search states.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpace[S] {

  def succ(x: S): Traversable[S]

  def depthFirstTreeTraversal(start: S): Enumerable[S] =
    Enumerable.ofEnumerator(new DepthFirstTreeSearchEnumerator[S](start)(this))

  def breadthFirstTreeTraversal(start: S): Enumerable[S] =
    Enumerable.ofEnumerator(new BreadthFirstTreeSearchEnumerator[S](start)(this))

}
