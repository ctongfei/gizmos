package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.collection._

/**
 * Defines a space of search states.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait StateSpace[S] extends Keyed[S] {

  /** Returns the successive states of the specified state under this state space. */
  def succ(x: S): Traversable[S]

  /** Returns the equivalence relation on search states. */
  def equivOnState: Equiv[S]

  def equivOnKey = equivOnState

  def depthFirstTreeTraversal(s: S) = Iterable.ofIterator(new DepthFirstTreeIterator(this, s))

  def breadthFirstTreeTraversal(s: S) = Iterable.ofIterator(new BreadthFirstTreeIterator(this, s))

  def depthFirstGraphTraversal(s: S) = Iterable.ofIterator(new DepthFirstIterator(this, s))

  def breadthFirstGraphTraversal(s: S) = Iterable.ofIterator(new DepthFirstIterator(this, s))

}
