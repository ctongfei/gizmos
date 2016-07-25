package poly.collection.search

import poly.collection._
import poly.collection.mut._
import poly.collection.search.node._
import poly.collection.search.ops._

/**
  * An extremely generic search iterator that executes a search algorithm on a generic state space.
  * @tparam S Type of state
  * @tparam N Type of search node
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
abstract class Searcher[S, N](val fringe: Queue[N], val start: S) extends SearchIterator[N, S] {

  /** Dictates whether a node should be pruned in the searching process. */
  def prune(n: N): Boolean

  /** The state space in which the searching process is performed. */
  implicit def stateSpace: StateSpace[S]

  /** Encapsulates the relation between underlying states and wrapping search nodes. */
  implicit def searchNodeInfo: SearchNodeInfo[N, S]

  private[this] var curr: N = default[N]

  fringe += searchNodeInfo.startNode(start) // at startup time, pushes the starting state into the fringe.

  /** Returns the current node of this search iterator. */
  def currentNode = curr

  /** Returns the current state of this search iterator. */
  def current = curr.state

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      if (!prune(curr))
        fringe ++= curr.state.succ map curr.next
      true
    }
    else false
  }
}

class DepthFirstTreeIterator[S](val stateSpace: StateSpace[S], start: S) extends Searcher[S, S](ArrayStack[S](), start) {
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class BreadthFirstTreeIterator[S](val stateSpace: StateSpace[S], start: S) extends Searcher[S, S](ArrayQueue[S](), start) {
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class DepthFirstIterator[S](val stateSpace: EquatableStateSpace[S], start: S) extends Searcher[S, S](
  DistinctQueue[ArrayStack, S]()(stateSpace.eqOnKeys, ArrayStack.newBuilder), start
) {
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class BreadthFirstIterator[S](val stateSpace: EquatableStateSpace[S], start: S) extends Searcher[S, S](
  DistinctQueue[ArrayQueue, S]()(stateSpace.eqOnKeys, ArrayQueue.newBuilder), start
) {
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class DepthFirstBacktrackableIterator[S](val stateSpace: EquatableStateSpace[S], start: S) extends Searcher[S, WithParent[S]](
  DistinctQueue[ArrayStack, WithParent[S]]()(stateSpace.eqOnKeys contramap { _.state }, ArrayStack.newBuilder), start
) {
  def prune(n: WithParent[S]) = false
  def searchNodeInfo = WithParent.SearchNodeInfo[S]
}

class BreadthFirstBacktrackableIterator[S](val stateSpace: EquatableStateSpace[S], start: S) extends Searcher[S, WithParent[S]](
  DistinctQueue[ArrayQueue, WithParent[S]]()(stateSpace.eqOnKeys contramap { _.state }, ArrayQueue.newBuilder), start
) {
  def prune(n: WithParent[S]) = false
  def searchNodeInfo = WithParent.SearchNodeInfo[S]
}
