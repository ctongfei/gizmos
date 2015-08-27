package poly.collection.search

import poly.algebra._
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
class TreeSearchWithCostEnumerator[S, C](
  val start: S,
  val fringe: Queue[SearchNodeWithCost[S, C]]
)(implicit ss: StateSpaceWithCost[S, C]) extends Enumerator[S] {

  private implicit def numericOfCost = ss.numericOfCost

  private[this] var curr: SearchNodeWithCost[S, C] = SearchNodeWithCost.dummy[C]

  fringe += SearchNodeWithCost(start, 0, numericOfCost.zero, curr)

  def current = curr.state

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succWithCost(curr.state).map { case (s, cost) =>
        SearchNodeWithCost(s, curr.depth + 1, curr.g + cost, curr)
      }
      true
    }
    else false
  }

}

class UniformCostTreeSearchEnumerator[S, C](start: S)(implicit ss: StateSpaceWithCost[S, C])
  extends TreeSearchWithCostEnumerator[S, C](
    start,
    BinaryHeapPriorityQueue[SearchNodeWithCost[S, C]]()(SearchNodeWithCost.order(ss.numericOfCost))
  )(ss)
