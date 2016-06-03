package poly.collection.search

import poly.collection.search.node._

/**
  * @author Tongfei Chen
  */
trait NodePruning[-N] extends (N => Boolean) {
  /** Dictates whether a node should be expanded in a searching process. */
  def apply(n: N): Boolean
}

object NodePruning {
  object None extends NodePruning[Any] {
    def apply(n: Any) = true
  }

  def DepthLimited[S](maxDepth: Int): NodePruning[WithParent[S]] = new NodePruning[WithParent[S]] {
    def apply(n: WithParent[S]) = n.depth <= maxDepth
  }

}
