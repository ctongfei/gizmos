package poly.collection.search

import poly.collection._
import poly.collection.mut._
import poly.collection.search.node._
import poly.collection.search.ops._

/**
  * An extremely generic search iterator that executes a weighted search algorithm.
  *
  * @tparam S Type of state
  * @tparam N Type of search node
  * @tparam C Type of cost on edges
  * @author Yuhuan Jiang
  * @author Tongfei Chen
  * @since 0.1.0
  */
abstract class WeightedSearcher[S, N, C](val fringe: KeyedPriorityQueue[S, N], val start: S) extends SearchIterator[S, N] {

  def prune(n: N): Boolean

  /** The state space in which the search process is performed. */
  implicit def stateSpace: WeightedStateSpace[S, C]

  /** Encapsulates the relation between underlying states and wrapping search nodes. */
  implicit def searchNodeInfo: WeightedSearchNodeInfo[N, S, C]

  private[this] var curr: N = default[N]

  fringe += searchNodeInfo.startNode(start)

  def currentNode = curr

  def current = searchNodeInfo.state(curr)

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.dequeue()
      if (!prune(curr))
        fringe ++= curr.state.succWithCost.map { case (next, cost) =>
          curr.next(next, cost)
        }
      true
    }
    else false
  }
}

class UniformCostIterator[S, C: Order : AdditiveMonoid](val stateSpace: WeightedStateSpace[S, C], start: S)
  extends WeightedSearcher[S, WithCost[S, C], C](
    DistinctPriorityQueue[S, WithCost[S, C]](BinaryHeap(), _.state)(stateSpace.keyEq),
    start)
{
  def searchNodeInfo = WithCost.WeightedSearchNodeInfo[S, C]
  def prune(n: WithCost[S, C]) = false
}

class GreedyBestFirstIterator[S, C: Order : AdditiveMonoid](val stateSpace: WeightedStateSpace[S, C], start: S, val heuristic: S => C)
  extends WeightedSearcher[S, WithHeuristic[S, C], C](
    DistinctPriorityQueue[S, WithHeuristic[S, C]](BinaryHeap(), _.state)(stateSpace.keyEq),
    start)
{
  def searchNodeInfo = WithHeuristic.WeightedSearchNodeInfo(heuristic)
  def prune(n: WithHeuristic[S, C]) = false
}

class AStarIterator[S, C: Order : AdditiveMonoid](val stateSpace: WeightedStateSpace[S, C], start: S, val heuristic: S => C)
  extends WeightedSearcher[S, WithCostAndHeuristic[S, C], C](
    DistinctPriorityQueue[S, WithCostAndHeuristic[S, C]](BinaryHeap()(WithCostAndHeuristic.order), _.state)(stateSpace.keyEq),
    start) { //TODO: contravariant typeclass implicit resolution bug SI-2509
  def searchNodeInfo = WithCostAndHeuristic.WeightedSearchNodeInfo(heuristic)
  def prune(n: WithCostAndHeuristic[S, C]) = false
}
