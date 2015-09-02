package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.function._
import poly.collection._
import poly.collection.mut._
import poly.collection.node._

/**
 * An enumerator that executes greedy best first search on trees given a state space with cost.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class GreedyBestFirstTreeSearchIterator[S, C: WeakOrder: AdditiveMonoid]
  (val start: S)
  (implicit ss: StateSpaceWithHeuristic[S, C])
  extends SearchIterator[S]
{

  val fringe = BinaryHeapPriorityQueue[SearchNodeWithHeuristic[S, C]]()(SearchNodeWithHeuristic.order)

  private implicit def numericOfCost = ss.monoidOfCost

  private[this] var curr: SearchNodeWithHeuristic[S, C] = SearchNodeWithHeuristic.dummy[C]

  fringe += SearchNodeWithHeuristic(start, 0, ss.heuristic(start), curr)

  def currentNode = curr

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succ(curr.state).map(s => SearchNodeWithHeuristic(s, curr.depth + 1, ss.heuristic(s), curr))
      true
    }
    else false
  }

}
