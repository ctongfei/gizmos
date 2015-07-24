package poly.collection.graph

//TODO: import poly.collection._
/**
 * The base trait for graph builders, which are objects that allow
 * incremental construction of graphs.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait GraphBuilder[-I, -V, -E, +G] {

  /**
   * Provides a hint to this builder about how many vertices are expected to be added.
   * @param n The hint how many vertices is to be added
   */
  def numVerticesHint(n: Int): Unit

  def addVertex(i: I, v: V): Unit

  def addEdge(i: I, j: I, e: E): Unit

  def addVertices(ivs: Traversable[(I, V)]) = ivs foreach { case (i, v) => addVertex(i, v) }

  def addEdges(iies: Traversable[(I, I, E)]) = iies foreach { case (i, j, e) => addEdge(i, j, e) }

  def result: G

}
