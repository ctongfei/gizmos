package poly.collection.search

import poly.collection._
import poly.collection.mut._
import poly.collection.search.node._

/**
  * An extremely generic iterator that executes a search algorithm.
 *
  * @tparam S Type of state
  * @tparam N Type of search node
  * @param S The searching state space
  * @param N A typeclass instance that witnesses the additional information stored on search nodes
  * @param pruningStrategy A typeclass instance that dictates which nodes should be pruned in the searching process
  * @param fringe A fringe for storing the search nodes
  * @param start Starting state
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
abstract class Searcher[S, N](
  pruningStrategy: NodePruning[N],
  fringe: Queue[N],
  start: S)
  (implicit S: StateSpace[S], N: SearchNodeInfo[N, S]) extends SearchIterator[N, S] {

  private[this] var curr: N = default[N]

  fringe += N.startNode(start)

  def currentNode = curr

  def current = N.state(curr)

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      if (!pruningStrategy.shouldBePruned(curr))
        fringe ++= S.succ(N.state(curr)).map(N.nextNode(curr))
      true
    }
    else false
  }
}

class DepthFirstTreeIterator[S](ss: StateSpace[S], start: S)
  extends Searcher[S, S](NodePruning.None, ArrayStack[S](), start)(ss, SearchNodeInfo.None)

class BreadthFirstTreeIterator[S](ss: StateSpace[S], start: S)
  extends Searcher[S, S](NodePruning.None, ArrayQueue[S](), start)(ss, SearchNodeInfo.None)

class DepthFirstIterator[S](ss: StateSpace[S], start: S)
  extends Searcher[S, S](NodePruning.None, DistinctQueue[ArrayStack, S](), start)(ss, SearchNodeInfo.None)

class BreadthFirstIterator[S](ss: StateSpace[S], start: S)
  extends Searcher[S, S](NodePruning.None, DistinctQueue[ArrayQueue, S](), start)(ss, SearchNodeInfo.None)

class DepthFirstBacktrackableIterator[S](ss: StateSpace[S], start: S)
  extends Searcher[S, WithParent[S]](NodePruning.None, DistinctQueue[ArrayStack, WithParent[S]](), start)(ss, WithParent.SearchNodeInfo[S])

class BreadthFirstBacktrackableIterator[S](ss: StateSpace[S], start: S)
  extends Searcher[S, WithParent[S]](NodePruning.None, DistinctQueue[ArrayQueue, WithParent[S]](), start)(ss, WithParent.SearchNodeInfo[S])

