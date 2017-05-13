package poly.collection.search.node

import spire.syntax.additiveMonoid._
import poly.collection._
import poly.collection.exception._
import poly.collection.search._

/**
 * Represents a node in the fringe / open set of a searching algorithm.
 *
 * @author Yuhuan Jiang
 * @author Tongfei Chen
 */
trait WithCost[S, C] extends WithParent[S] {
  def state: S
  def parent: WithCost[S, C]
  /** The known cost from the initial node to this node. */
  def g: C
}

object WithCost extends WithCostLowPriorityImplicit {

  def apply[S, C](s: S, d: Int, gv: C, p: WithCost[S, C]): WithCost[S, C] = new WithCost[S, C] {
    val state = s
    val depth = d
    val g = gv
    val parent = p
    def isDummy = false
  }

  def dummy[S, C: Order : AdditiveMonoid]: WithCost[S, C] = new WithCost[S, C] {
    def state: Nothing = throw new DummyNodeException
    val depth: Int = -1
    val g = zero[C]
    def parent = this
    def isDummy = true
  }

  implicit def WeightedSearchNodeInfo[S, C: Order : AdditiveMonoid]: WeightedSearchNodeInfo[WithCost[S, C], S, C] =
    new WeightedSearchNodeInfo[WithCost[S, C], S, C] {
      def startNode(s: S) = WithCost(s, 0, zero[C], dummy[S, C])
      def state(n: WithCost[S, C]) = n.state
      def nextNode(currNode: WithCost[S, C])(nextState: S, cost: C) = WithCost(nextState, currNode.depth + 1, currNode.g + cost, currNode)
    }
}

// This should have lower priority than SearchNodeWithHeuristic.order
private[collection] trait WithCostLowPriorityImplicit {
  implicit def order[S, C: Order : AdditiveMonoid]: Order[WithCost[S, C]] = Order by { _.g }
}
