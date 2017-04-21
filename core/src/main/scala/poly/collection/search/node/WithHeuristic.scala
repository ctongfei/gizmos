package poly.collection.search.node

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.exception._
import poly.collection.search._

/**
  * Represents a node in the fringe / open set of a searching algorithm.
  *
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  * @author Tongfei Chen
  */
trait WithHeuristic[S, C] extends WithParent[S] {

  def parent: WithHeuristic[S, C]

  /** The heuristic estimate of the cost from this node to the goal. */
  def h: C

}


object WithHeuristic extends WithHeuristicLowPriorityImplicits {

  def apply[S, C](s: S, d: Int, hv: C, p: WithHeuristic[S, C]): WithHeuristic[S, C] = new WithHeuristic[S, C] {
    val state = s
    val depth = d
    val h = hv
    val parent = p
    def isDummy = false
  }

  def dummy[S, C: OrderedAdditiveGroup]: WithHeuristic[S, C] = new WithHeuristic[S, C] {
    def state: Nothing = throw new DummyNodeException
    val depth: Int = -1
    val h = zero[C]
    def parent = this
    def isDummy = true
  }

  def WeightedSearchNodeInfo[S, C: OrderedAdditiveGroup](h: S => C): WeightedSearchNodeInfo[WithHeuristic[S, C], S, C]
  = new WeightedSearchNodeInfo[WithHeuristic[S, C], S, C] {
    def startNode(s: S) = WithHeuristic(s, 0, h(s), dummy[S, C])
    def state(n: WithHeuristic[S, C]) = n.state
    def nextNode(currNode: WithHeuristic[S, C])(nextState: S, cost: C) = WithHeuristic(nextState, currNode.depth + 1, h(nextState), currNode)
  }
}

private[collection] trait WithHeuristicLowPriorityImplicits {
  implicit def order[S, C: OrderedAdditiveGroup]: Order[WithHeuristic[S, C]] = Order.by(_.h)
}
