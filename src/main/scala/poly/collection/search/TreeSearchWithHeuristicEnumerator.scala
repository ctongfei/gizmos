package poly.collection.search

import poly.algebra.ops._
import poly.algebra.function._
import poly.collection._
import poly.collection.mut._
import poly.collection.node._

/**
 * An enumerator that executes a search on trees (assumes that there's no loop).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class TreeSearchWithHeuristicEnumerator[S, C](
  val start: S,
  val fringe: Queue[SearchNodeWithHeuristic[S, C]]
)(implicit ss: StateSpaceWithHeuristic[S, C]) extends Enumerator[S] {

  private implicit def numericOfCost = ss.numericOfCost

  private[this] var curr: SearchNodeWithHeuristic[S, C] = SearchNodeWithHeuristic.dummy[C]

  fringe += SearchNodeWithHeuristic(start, 0, zero[C], ss.heuristic(start), curr)

  def current = curr.state

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succWithCost(curr.state).map { case (s, cost) =>
        SearchNodeWithHeuristic(s, curr.depth + 1, curr.g + cost, ss.heuristic(s), curr)
      }
      true
    }
    else false
  }

}

class AStarTreeSearchEnumerator[S, C](start: S)(implicit ss: StateSpaceWithHeuristic[S, C])
  extends TreeSearchWithHeuristicEnumerator[S, C](
    start,
    BinaryHeapPriorityQueue[SearchNodeWithHeuristic[S, C]]()(SearchNodeWithHeuristic.order(ss.numericOfCost))
  )(ss)
