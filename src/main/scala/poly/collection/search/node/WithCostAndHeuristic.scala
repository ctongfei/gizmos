package poly.collection.search.node

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.exception._
import poly.collection.search._

/**
 * Represents a node in the fringe / open set of a searching algorithm.
 *
 * @author Yuhuan Jiang
 * @author Tongfei Chen
 */
trait WithCostAndHeuristic[S, C] extends WithCost[S, C] with WithHeuristic[S, C] {

  def parent: WithCostAndHeuristic[S, C]

  def f(implicit C: OrderedAdditiveGroup[C]) = g + h
}


object WithCostAndHeuristic {

  implicit def order[S, C: OrderedAdditiveGroup]: Order[WithCostAndHeuristic[S, C]] = Order.by(_.f)

  def apply[S, C](s: S, d: Int, gv: C, hv: C, p: WithCostAndHeuristic[S, C]): WithCostAndHeuristic[S, C] = new WithCostAndHeuristic[S, C] {
    val state = s
    val depth = d
    val g = gv
    val h = hv
    val parent = p
    def isDummy = false
  }

  def dummy[S, C: OrderedAdditiveGroup]: WithCostAndHeuristic[S, C] = new WithCostAndHeuristic[S, C] {
    def state: Nothing = throw new DummyNodeException
    val depth: Int = -1
    val g = zero[C]
    val h = zero[C]
    def parent = this
    def isDummy = true
  }

  def WeightedSearchNodeInfo[S, C: OrderedAdditiveGroup](heuristic: S => C): WeightedSearchNodeInfo[WithCostAndHeuristic[S, C], S, C]
    = new WeightedSearchNodeInfo[WithCostAndHeuristic[S, C], S, C] {
    def startNode(s: S) = WithCostAndHeuristic(s, 0, zero[C], heuristic(s), dummy[S, C])
    def state(n: WithCostAndHeuristic[S, C]) = n.state
    def nextNode(currNode: WithCostAndHeuristic[S, C])(nextState: S, cost: C) = WithCostAndHeuristic(nextState, currNode.depth + 1, currNode.g + cost, heuristic(nextState), currNode)
  }
}

