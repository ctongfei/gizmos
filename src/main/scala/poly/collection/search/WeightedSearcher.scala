package poly.collection.search

import poly.algebra._
import poly.collection.exception._
import poly.collection.mut._
import poly.collection.search.node._

/**
  * An extremely generic iterator that executes a weighted search algorithm.
  *
  * @tparam S Type of state
  * @tparam N Type of search node
  * @param S The searching state space
  * @param N A typeclass instance that witnesses the additional information stored on search nodes
  * @param pruning A typeclass instance that dictates which nodes should be pruned in the searching process
  * @param fringe A fringe for storing the search nodes
  * @param start Starting state
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.1.0
  */
class WeightedSearcher[S, N, C](
  pruning: NodePruning[N],
  fringe: Queue[N],
  start: S
)(implicit
  S: WeightedStateSpace[S, C],
  N: WeightedSearchNodeInfo[N, S, C]
) extends SearchIterator[S, N] {

  private[this] var curr: N = throw new DummyNodeException

  fringe += N.startNode(start)

  def currentNode = curr
  def currentState = N.state(curr)

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      if (!pruning.shouldBePruned(curr))
        fringe ++= S.succWithCost(N.state(curr)).map { case (next, cost) =>
          N.nextNode(curr)(next, cost)
        }
      true
    }
    else false
  }
}

class UniformCostIterator[S, C: OrderedAdditiveGroup](ss: WeightedStateSpace[S, C], start: S)
  extends WeightedSearcher[S, WithCost[S, C], C](NodePruning.None, BinaryHeapPriorityQueue[WithCost[S, C]](), start)(ss, WithCost.WeightedSearchNodeInfo)

