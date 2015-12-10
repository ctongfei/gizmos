package poly.collection.search

import poly.collection.search.node._

/**
  * @author Tongfei Chen
  */
trait NodePruning[-N] {
  /** Dictates whether a node should be expanded in a searching process. */
  def shouldBePruned(n: N): Boolean
}

object NodePruning {
  object None extends NodePruning[Any] {
    def shouldBePruned(n: Any) = false
  }

  def DepthLimited[S](maxDepth: Int): NodePruning[WithParent[S]] = new NodePruning[WithParent[S]] {
    def shouldBePruned(n: WithParent[S]) = n.depth >= maxDepth
  }

}
