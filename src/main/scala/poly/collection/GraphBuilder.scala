package poly.collection

/**
 * The base trait for graph builders, which are objects that allow
 * incremental construction of graphs.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait GraphBuilder[-K, -V, -E, +G] {

  /**
   * Provides a hint to this builder about how many vertices are expected to be added.
   * @param n The hint how many vertices is to be added
   */
  def numVerticesHint(n: Int): Unit

  def addVertex(i: K, v: V): Unit

  def addEdge(i: K, j: K, e: E): Unit

  def addVertices(kvs: Traversable[(K, V)]) = kvs foreach { case (i, v) => addVertex(i, v) }

  def addEdges(kkes: Traversable[(K, K, E)]) = kkes foreach { case (i, j, e) => addEdge(i, j, e) }

  def result: G

}
