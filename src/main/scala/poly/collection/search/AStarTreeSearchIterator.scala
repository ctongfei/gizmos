package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection._
import poly.collection.mut._
import poly.collection.node._

/**
 * An enumerator that executes A* search on trees given a state space with cost and a heuristic function.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class AStarTreeSearchIterator[S, C: WeakOrder: AdditiveMonoid]
  (val start: S)
  (implicit ss: StateSpaceWithCostAndHeuristic[S, C])
  extends SearchIterator[S]
{

  val fringe = BinaryHeapPriorityQueue[SearchNodeWithCostAndHeuristic[S, C]]()(SearchNodeWithCostAndHeuristic.order)

  private[this] var curr: SearchNodeWithCostAndHeuristic[S, C] = SearchNodeWithCostAndHeuristic.dummy[C]

  fringe += SearchNodeWithCostAndHeuristic(start, 0, zero, ss.heuristic(start), curr)

  def currentNode = curr

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succWithCost(curr.state).map { case (s, cost) =>
        SearchNodeWithCostAndHeuristic(s, curr.depth + 1, curr.g + cost, ss.heuristic(s), curr)
      }
      true
    }
    else false
  }

}
