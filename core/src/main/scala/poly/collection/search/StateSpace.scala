package poly.collection.search

import poly.collection._

/**
 * Represents a simple state space that is specified by a state transition function.
 * @author Tongfei Chen
 * @author Yuhuan Jiang
 * @since 0.1.0
 */
trait StateSpace[S] { self =>

  /** Returns the successive states of the specified state under this state space. */
  def succ(x: S): Traversable[S]

  /** Constraints this state space by selecting only the states that satisfy the given predicate. */
  def filterKeys(f: S => Boolean): StateSpace[S] = new StateSpaceT.KeyFiltered(self, f)

  /** Constraints this state space by selecting only the states that satisfy the given predicate. */
  def filter(f: S => Boolean) = filterKeys(f)

  /**
   * Depth-first traverses this state space from the given starting state.
   * $LAZY */
  def depthFirstTreeTraversal(start: S) =
    Iterable.ofIterator(new DepthFirstTreeIterator(this, start))

  /** Breadth-first traverses this state space from the given starting state.
   * $LAZY */
  def breadthFirstTreeTraversal(start: S) =
    Iterable.ofIterator(new BreadthFirstTreeIterator(this, start))

}

object StateSpace {

  /** Creates a state space given a state transition function. */
  def apply[S](f: S => Traversable[S]): StateSpace[S] = new StateSpaceT.BySucc(f)

}

private[poly] object StateSpaceT {

  class BySucc[S](f: S => Traversable[S]) extends StateSpace[S] {
    def succ(x: S) = f(x)
  }

  class KeyFiltered[S](self: StateSpace[S], f: S => Boolean) extends StateSpace[S] {
    def succ(x: S) = self.succ(x) filter f
  }

}
