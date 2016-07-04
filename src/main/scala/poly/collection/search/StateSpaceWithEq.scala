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
trait StateSpaceWithEq[@sp(Int) S] extends StateSpace[S] with Keyed[S] { self =>

  import StateSpaceWithEq._

  /** Returns the equivalence relation on search states. */
  implicit def eqOnKeys: Eq[S]

  // HELPER FUNCTIONS

  /** Constraints this state space by selecting only the states that satisfy the given predicate. */
  def filterKeys(f: S => Boolean): StateSpaceWithEq[S] = new StateSpaceWithEqT.KeyFiltered(self, f)

  def depthFirstTraversal(start: S) =
    Iterable.ofIterator(new DepthFirstIterator(this, start))

  def breadthFirstTraversal(start: S) =
    Iterable.ofIterator(new BreadthFirstIterator(this, start))

  /** Finds a path to a state that satisfy the given condition from a specified starting state using depth-first search. */
  def depthFirstSearch(start: S)(goal: S => Boolean): BiSeq[S] =
    searchByIterator(new DepthFirstBacktrackableIterator(this, start), goal)

  /** Finds the path between two states using depth-first search. */
  def depthFirstSearch(start: S, goal: S): BiSeq[S] =
    depthFirstSearch(start)(goal === _)

  /** Finds a path to a state that satisfy the given condition from a specified starting state using breadth-first search. */
  def breadthFirstSearch(start: S)(goal: S => Boolean): BiSeq[S] =
    searchByIterator(new BreadthFirstBacktrackableIterator(this, start), goal)

  /** Finds the path between two states using breadth-first search. */
  def breadthFirstSearch(start: S, goal: S): BiSeq[S] =
    breadthFirstSearch(start)(goal === _)

}

object StateSpaceWithEq {

  private[collection] def searchByIterator[S, N <: node.WithParent[S]](si: SearchIterator[N, S], goal: S => Boolean): BiSeq[S] = {
    while (si.advance())
      if (goal(si.current))
        return si.currentNode.pathToRoot.map(_.data).reverse
    BiSeq.empty
  }


  def create[S](f: S => Traversable[S])(implicit S: Eq[S]): StateSpaceWithEq[S] = new StateSpaceWithEqT.BySucc(f, S)

}

abstract class AbstractStateSpaceWithEq[@sp(Int) S] extends StateSpaceWithEq[S]

private[poly] object StateSpaceWithEqT {

  class BySucc[S](f: S => Traversable[S], val eq: Eq[S]) extends AbstractStateSpaceWithEq[S] {
    /** Returns the equivalence relation on search states. */
    def eqOnKeys = eq
    def succ(x: S) = f(x)
  }

  class KeyFiltered[S](self: StateSpaceWithEq[S], f: S => Boolean) extends AbstractStateSpaceWithEq[S] {
    def eqOnKeys = self.eqOnKeys
    def succ(x: S) = self.succ(x) filter f
  }

}