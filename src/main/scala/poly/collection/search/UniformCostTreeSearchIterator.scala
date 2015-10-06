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
class UniformCostTreeSearchIterator[S, C]
  (val start: S)
  (implicit ss: StateSpaceWithCost[S, C])
  extends SearchIterator[S]
{

  implicit def groupOnCost = ss.groupOnCost

  val fringe = BinaryHeapPriorityQueue[SearchNodeWithCost[S, C]]()(SearchNodeWithCost.order)

  private[this] var curr: SearchNodeWithCost[S, C] = SearchNodeWithCost.dummy[C]

  fringe += SearchNodeWithCost(start, 0, zero, curr)

  def currentNode = curr

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
