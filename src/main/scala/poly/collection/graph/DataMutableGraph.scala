package poly.collection.graph

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait DataMutableGraph[I, V, E] extends Graph[I, V, E] {

  def update(i: I, v: V): Unit
  def update(i: I, j: I, e: E): Unit

}
