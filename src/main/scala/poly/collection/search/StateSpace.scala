package poly.collection.search

import poly.collection._

/**
 * Defines a space of search states.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpace[S] {

  /** Returns the successor states of the specified state under this state space. */
  def succ(x: S): Traversable[S]

  def depthFirstTreeTraversal(start: S): Iterable[S] =
    Iterable.ofIterator(new DepthFirstTreeSearchIterator[S](start)(this))

  def breadthFirstTreeTraversal(start: S): Iterable[S] =
    Iterable.ofIterator(new BreadthFirstTreeSearchIterator[S](start)(this))


}
