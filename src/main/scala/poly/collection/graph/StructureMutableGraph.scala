package poly.collection.graph

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StructureMutableGraph[V, E] extends DataMutableGraph[V, E] {

  def addVertex(v: V): Unit
  def deleteVertex(i: Int): Unit

  def insertEdge(i: Int, j: Int, e: E): Unit
  def deleteEdge(i: Int, j: Int): Unit

}

trait StructureMutableDirectedGraph[V, E] extends DirectedGraph[V, E] with StructureMutableGraph[V, E]

trait StructureMutableUndirectedGraph[V, E] extends UndirectedGraph[V, E] with StructureMutableGraph[V, E]
