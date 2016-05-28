package poly.collection.search

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection._

/**
  * Represents a space of search states, which can be considered as an implicit graph.
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait StateSpace[@sp(Int) S] extends Keyed[S] { self ⇒

  import StateSpace._

  /** Returns the successive states of the specified state under this state space. */
  def succ(x: S): Traversable[S]

  /** Returns the equivalence relation on search states. */
  implicit def eqOnKeys: Eq[S]

  // HELPER FUNCTIONS

  /** Constraints this state space by selecting only the states that satisfy the given predicate. */
  def filterKeys(f: S ⇒ Boolean): StateSpace[S] = new AbstractStateSpace[S] {
    def eqOnKeys = self.eqOnKeys
    def succ(x: S) = self.succ(x) filter f
  }

  /**
   * Depth-first traverses this state space from the given starting state.
   * This method uses tree traversal: The user must guarantee that the state space is a tree.
   * Otherwise, use the [[depthFirstTraversal]] method.
   * $LAZY */
  def depthFirstTreeTraversal(start: S) =
    Iterable.ofIterator(new DepthFirstTreeIterator(this, start))

  /** Breadth-first traverses this state space from the given starting state.
   * This method uses tree traversal: The user must guarantee that the state space is a tree.
   * Otherwise, use the [[breadthFirstTraversal]] method.
   * $LAZY */
  def breadthFirstTreeTraversal(start: S) =
    Iterable.ofIterator(new BreadthFirstTreeIterator(this, start))

  def depthFirstTraversal(start: S) =
    Iterable.ofIterator(new DepthFirstIterator(this, start))

  def breadthFirstTraversal(start: S) =
    Iterable.ofIterator(new BreadthFirstIterator(this, start))

  /** Finds a path to a state that satisfy the given condition from a specified starting state using depth-first search. */
  def depthFirstSearch(start: S)(goal: S ⇒ Boolean): BiSeq[S] =
    searchByIterator(new DepthFirstBacktrackableIterator(this, start), goal)

  /** Finds the path between two states using depth-first search. */
  def depthFirstSearch(start: S, goal: S): BiSeq[S] =
    depthFirstSearch(start)(goal === _)

  /** Finds a path to a state that satisfy the given condition from a specified starting state using breadth-first search. */
  def breadthFirstSearch(start: S)(goal: S ⇒ Boolean): BiSeq[S] =
    searchByIterator(new BreadthFirstBacktrackableIterator(this, start), goal)

  /** Finds the path between two states using breadth-first search. */
  def breadthFirstSearch(start: S, goal: S): BiSeq[S] =
    breadthFirstSearch(start)(goal === _)

}

object StateSpace {

  private[collection] def searchByIterator[S, N <: node.WithParent[S]](si: SearchIterator[N, S], goal: S ⇒ Boolean): BiSeq[S] = {
    while (si.advance())
      if (goal(si.current))
        return si.currentNode.pathToRoot.map(_.data).reverse
    BiSeq.empty
  }

}

abstract class AbstractStateSpace[@sp(Int) S] extends StateSpace[S]
