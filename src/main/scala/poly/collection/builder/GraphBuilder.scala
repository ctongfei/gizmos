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
 *
   * @param n The hint how many vertices is to be added
   */
  def numNodesHint(n: Int): Unit

  def addNodeInplace(i: K): Unit

  def addEdgeInplace(i: K, j: K, e: E): Unit

  def addEdges(kkes: Traversable[(K, K, E)]) = kkes foreach { case (i, j, e) => addEdgeInplace(i, j, e) }

  def result: G

}
