package poly.collection.builder

import poly.collection._

/**
 * The base trait for graph builders, which are objects that allow
 * incremental construction of graphs.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait GraphBuilder[-K, -E, +G] {

  /**
   * Provides a hint to this builder about how many vertices are expected to be added.
   * @param n The hint how many vertices is to be added
   */
  def numNodesHint(n: Int) = {}

  def numEdgesHint(n: Int) = {}

  def addNodeInplace(i: K): Unit

  def addEdgeInplace(i: K, j: K, e: E): Unit

  def addNodes(ks: Traversable[K]) = {
    if (ks.sizeKnown)
      numNodesHint(ks.size)
    ks foreach addNodeInplace
  }

  def addEdges(kkes: Traversable[(K, K, E)]) = {
    if (kkes.sizeKnown)
      numEdgesHint(kkes.size)
    kkes foreach { case (i, j, e) => addEdgeInplace(i, j, e) }
  }

  def result: G

}
