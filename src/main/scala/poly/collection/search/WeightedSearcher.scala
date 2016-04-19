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
  * @tparam C Type of cost on edges
  * @param S The searching state space
  * @param N A typeclass instance that witnesses the additional information stored on search nodes
  * @param shouldNotBePruned A predicate that dictates which nodes should be not pruned in the searching process
  * @param fringe A fringe for storing the search nodes
  * @param start Starting state
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
class WeightedSearcher[S, N, C](
  shouldNotBePruned: N => Boolean,
  fringe: Queue[N],
  start: S
)(implicit
  S: WeightedStateSpace[S, C],
  N: WeightedSearchNodeInfo[N, S, C]
) extends SearchIterator[N, S] {

  private[this] var curr: N = throw new DummyNodeException

  fringe += N.startNode(start)

  def currentNode = curr

  def current = N.state(curr)

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      if (shouldNotBePruned(curr))
        fringe ++= S.succWithCost(N.state(curr)).map { case (next, cost) =>
          N.nextNode(curr)(next, cost)
        }
      true
    }
    else false
  }
}

class UniformCostIterator[S, C: OrderedAdditiveGroup](ss: WeightedStateSpace[S, C], start: S)
  extends WeightedSearcher[S, WithCost[S, C], C](
    x => false,
    DistinctPriorityQueue[BinaryHeap, WithCost[S, C]](),
    start)(ss, WithCost.WeightedSearchNodeInfo)

class GreedyBestFirstIterator[S, C: OrderedAdditiveGroup](ss: WeightedStateSpace[S, C], start: S, heuristic: S => C)
  extends WeightedSearcher[S, WithHeuristic[S, C], C](
    x => false,
    DistinctPriorityQueue[BinaryHeap, WithHeuristic[S, C]](),
    start)(ss, WithHeuristic.WeightedSearchNodeInfo(heuristic)
  )

class AStarIterator[S, C: OrderedAdditiveGroup](ss: WeightedStateSpace[S, C], start: S, heuristic: S => C)
  extends WeightedSearcher[S, WithCostAndHeuristic[S, C], C](
    x => false,
    DistinctPriorityQueue[BinaryHeap, WithCostAndHeuristic[S, C]]()(Equiv.byRef, BinaryHeap.newBuilder[WithCostAndHeuristic[S, C]](WithCostAndHeuristic.order)),
    start)(ss, WithCostAndHeuristic.WeightedSearchNodeInfo(heuristic))
