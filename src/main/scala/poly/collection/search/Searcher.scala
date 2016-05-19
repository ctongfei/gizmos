package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.collection.mut._
import poly.collection.search.node._
import poly.collection.search.ops._

/**
  * An extremely generic iterator that executes a search algorithm on a generic state space.
  * @tparam S Type of state
  * @tparam N Type of search node
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
abstract class Searcher[S, N] extends SearchIterator[N, S] {

  /** Dictates whether a node should be pruned in the searching process. */
  def prune(n: N): Boolean

  val fringe: Queue[N]

  val start: S

  /** The state space in which the searching process is performed. */
  implicit def stateSpace: StateSpace[S]

  /** Encapsulates the relation between underlying states and wrapping search nodes. */
  implicit def searchNodeInfo: SearchNodeInfo[N, S]

  private[this] var curr: N = default[N]

  fringe += searchNodeInfo.startNode(start)

  /** Returns the current node of this search iterator. */
  def currentNode = curr

  /** Returns the current state of this search iterator. */
  def current = searchNodeInfo.state(curr)

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

class DepthFirstTreeIterator[S](val stateSpace: StateSpace[S], val start: S) extends Searcher[S, S] {
  val fringe = ArrayStack[S]()
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class BreadthFirstTreeIterator[S](val stateSpace: StateSpace[S], val start: S) extends Searcher[S, S] {
  val fringe = ArrayQueue[S]()
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class DepthFirstIterator[S](val stateSpace: StateSpace[S], val start: S) extends Searcher[S, S] {
  val fringe = DistinctQueue[ArrayStack, S]()(stateSpace.eqOnKeys, ArrayStack.newBuilder)
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class BreadthFirstIterator[S](val stateSpace: StateSpace[S], val start: S) extends Searcher[S, S] {
  val fringe = DistinctQueue[ArrayQueue, S]()(stateSpace.eqOnKeys, ArrayQueue.newBuilder)
  def prune(n: S) = false
  def searchNodeInfo = SearchNodeInfo.None
}

class DepthFirstBacktrackableIterator[S](val stateSpace: StateSpace[S], val start: S) extends Searcher[S, WithParent[S]] {
  val fringe = DistinctQueue[ArrayStack, WithParent[S]]()(stateSpace.eqOnKeys contramap { _.state }, ArrayStack.newBuilder)
  def prune(n: WithParent[S]) = false
  def searchNodeInfo = WithParent.SearchNodeInfo[S]
}

class BreadthFirstBacktrackableIterator[S](val stateSpace: StateSpace[S], val start: S) extends Searcher[S, WithParent[S]] {
  val fringe = DistinctQueue[ArrayQueue, WithParent[S]]()(stateSpace.eqOnKeys contramap { _.state }, ArrayQueue.newBuilder)
  def prune(n: WithParent[S]) = false
  def searchNodeInfo = WithParent.SearchNodeInfo[S]
}
