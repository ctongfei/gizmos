package poly.collection.graph

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StructureMutableGraph[I, V, E] extends DataMutableGraph[I, V, E] {

  def addVertex(i: I, v: V): Unit
  def removeVertex(i: I): Unit

  def addEdge(i: I, j: I, e: E): Unit
  def removeEdge(i: I, j: I): Unit

}
