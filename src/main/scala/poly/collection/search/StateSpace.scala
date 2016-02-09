package poly.collection.search

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._
import poly.collection.node._
import poly.collection.search.node._

/**
  * Represents a space of search states, which can be considered as an implicit graph.
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait StateSpace[@sp(i) S] extends Keyed[S] {

  import StateSpace._

  /** Returns the successive states of the specified state under this state space. */
  def succ(x: S): Traversable[S]

  /** Returns the equivalence relation on search states. */
  def equivOnKey: Equiv[S]

  def depthFirstTreeTraversal(start: S) = Iterable.ofIterator(new DepthFirstTreeIterator(this, start))

  def breadthFirstTreeTraversal(start: S) = Iterable.ofIterator(new BreadthFirstTreeIterator(this, start))

  def depthFirstTraversal(start: S) = Iterable.ofIterator(new DepthFirstIterator(this, start))

  def breadthFirstTraversal(start: S) = Iterable.ofIterator(new DepthFirstIterator(this, start))

  def depthFirstSearch(start: S, goal: S => Boolean) = searchByIterator(new DepthFirstBacktrackableIterator(this, start), goal)

  def breadthFirstSearch(start: S, goal: S => Boolean) = searchByIterator(new BreadthFirstBacktrackableIterator(this, start), goal)

}

object StateSpace {

  private[collection] def searchByIterator[S, N <: WithParent[S]](si: SearchIterator[N, S], goal: S => Boolean): BiSeq[S] = {
    while (si.advance())
      if (goal(si.current))
        return si.currentNode.pathToRoot.map(_.data).reverse
    BiSeq.empty
  }

}
