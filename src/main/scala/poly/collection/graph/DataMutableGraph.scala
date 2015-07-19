package poly.collection.graph

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait DataMutableGraph[V, E] extends Graph[V, E] {

  def update(i: Int, v: V): Unit
  def update(i: Int, j: Int, e: E): Unit

}

trait DataMutableDirectedGraph[V, E] extends DirectedGraph[V, E] with DataMutableGraph[V, E]

trait DataMutableUndirectedGraph[V, E] extends UndirectedGraph[V, E] with DataMutableGraph[V, E]
