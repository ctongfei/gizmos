package poly.collection.search

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection._

/**
 * Represents a space of equatable search states, which can be considered as an implicit graph.
 * In an '''''equatable''''' state space, there is an equivalence relation endowed on the set of states,
 * so two states can be considered ''equivalent'', henceforth allowing graph traversal (able to check loops!)
 * instead of tree traversal.
 * @author Yuhuan Jiang
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait EqStateSpace[@sp(Int) S] extends StateSpace[S] with Keyed[S] with Relation[S, S] { self =>

  import EqStateSpace._

  /** Returns the equivalence relation on search states. */
  implicit def keyEq: Eq[S]

  def related(x: S, y: S) = succ(x) contains y

  // HELPER FUNCTIONS

  /** Constraints this state space by selecting only the states that satisfy the given predicate. */
  override def filterKeys(f: S => Boolean): EqStateSpace[S] = new EqStateSpaceT.KeyFiltered(self, f)

  override def filter(f: S => Boolean) = filterKeys(f)

  /** Traverses this state space in a depth-first way, starting from the given state. */
  def depthFirstTraversal(start: S) =
    Iterable.ofIterator(new DepthFirstIterator(this, start))

  /** Traverses this state space in a breadth-first way, starting from the given state. */
  def breadthFirstTraversal(start: S) =
    Iterable.ofIterator(new BreadthFirstIterator(this, start))

  /** Finds a path to a state that satisfy the given condition from a specified starting state using depth-first search. */
  def depthFirstSearch(start: S)(goal: S => Boolean): BidiSeq[S] =
    searchByIterator(new DepthFirstBacktrackableIterator(this, start), goal)

  /** Finds the path between two states using depth-first search. */
  def depthFirstSearch(start: S, goal: S): BidiSeq[S] =
    depthFirstSearch(start)(goal === _)

  /** Finds a path to a state that satisfy the given condition from a specified starting state using breadth-first search. */
  def breadthFirstSearch(start: S)(goal: S => Boolean): BidiSeq[S] =
    searchByIterator(new BreadthFirstBacktrackableIterator(this, start), goal)

  /** Finds the path between two states using breadth-first search. */
  def breadthFirstSearch(start: S, goal: S): BidiSeq[S] =
    breadthFirstSearch(start)(goal === _)

}

object EqStateSpace {

  private[collection] def searchByIterator[S, N <: node.WithParent[S]](si: SearchIterator[N, S], goal: S => Boolean): BidiSeq[S] = {
    while (si.advance())
      if (goal(si.current))
        return si.currentNode.pathToRoot.map(_.data).reverse
    BidiSeq.empty
  }

  /**
   * Constructs an equatable state space by the given transition function.
   */
  def apply[S: Eq](f: S => Traversable[S]): EqStateSpace[S] = new EqStateSpaceT.BySucc(f, Eq[S])

}

abstract class AbstractEqStateSpace[@sp(Int) S] extends EqStateSpace[S]

private[poly] object EqStateSpaceT {

  class BySucc[S](f: S => Traversable[S], val eq: Eq[S]) extends AbstractEqStateSpace[S] {
    def keyEq = eq
    def succ(x: S) = f(x)
  }

  class KeyFiltered[S](self: EqStateSpace[S], f: S => Boolean) extends AbstractEqStateSpace[S] {
    def keyEq = self.keyEq
    def succ(x: S) = self.succ(x) filter f
  }

}