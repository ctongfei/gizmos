package poly.collection.search.node

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.search._
import poly.util.specgroup._

/**
 * Represents a node in the fringe / open set of a searching algorithm.
  *
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait WithHeuristic[S, @sp(fdi) C] extends WithCost[S, C] {

  def parent: WithHeuristic[S, C]

  /** The heuristic estimate of the cost from this node to the goal. */
  def h: C

  def f(implicit C: OrderedAdditiveGroup[C]) = g + h
}


object WithHeuristic {

  implicit def order[S, @sp(fdi) C: OrderedAdditiveGroup]: WeakOrder[WithHeuristic[S, C]] =
    new WeakOrder[WithHeuristic[S, C]] {
      def cmp(x: WithHeuristic[S, C], y: WithHeuristic[S, C]) = x.f >?< y.f
    }

  def apply[S, @sp(fdi) C](s: S, d: Int, gv: C, hv: C, p: WithHeuristic[S, C]): WithHeuristic[S, C] = new WithHeuristic[S, C] {
    val state = s
    val depth = d
    val g = gv
    val h = hv
    val parent = p
    def isDummy = false
  }

  def dummy[S, @sp(fdi) C: OrderedAdditiveGroup]: WithHeuristic[S, C] = new WithHeuristic[S, C] {
    def state: Nothing = throw new NoSuchElementException()
    val depth: Int = -1
    val g = zero[C]
    val h = zero[C]
    def parent = this
    def isDummy = true
  }

  def WeightedSearchNodeInfo[S, @sp(fdi) C: OrderedAdditiveGroup](h: S => C): WeightedSearchNodeInfo[WithHeuristic[S, C], S, C]
    = new WeightedSearchNodeInfo[WithHeuristic[S, C], S, C] {
    def startNode(s: S) = WithHeuristic(s, 0, zero[C], h(s), dummy[S, C])
    def state(n: WithHeuristic[S, C]) = n.state
    def nextNode(currNode: WithHeuristic[S, C])(nextState: S, cost: C) = WithHeuristic(nextState, currNode.depth + 1, currNode.g + cost, h(nextState), currNode)
  }
}

