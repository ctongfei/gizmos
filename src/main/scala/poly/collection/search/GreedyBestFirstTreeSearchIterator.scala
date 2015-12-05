package poly.collection.search

import poly.algebra._
import poly.algebra.ops._
import poly.algebra.specgroup._
import poly.collection.mut._
import poly.collection.node._

/**
 * An enumerator that executes greedy best first search on trees given a state space with cost.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class GreedyBestFirstTreeSearchIterator[S, @sp(fdi) C]
  (val start: S)(heuristic: S => C)
  (implicit ss: WeightedStateSpace[S, C])
  extends SearchIterator[S]
{

  implicit def groupOnCost = ss.groupOnCost

  val fringe = BinaryHeapPriorityQueue[SearchNodeWithHeuristic[S, C]]()

  private[this] var curr: SearchNodeWithHeuristic[S, C] = SearchNodeWithHeuristic.dummy[C]

  fringe += SearchNodeWithHeuristic(start, 0, zero[C], heuristic(start), curr)

  def currentNode = curr

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succ(curr.state).map { s =>
        SearchNodeWithHeuristic(s, curr.depth + 1, zero[C], heuristic(s), curr)
      }
      true
    }
    else false
  }

}
