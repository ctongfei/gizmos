package poly.collection

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection.mut._
import poly.collection.node._

/**
 * Represents an undirected graph.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait UndirectedGraph[@sp(Int) K, +E] extends BiGraph[K, E] { self =>

  import UndirectedGraph._

  def edge(i: K, j: K) = new EdgeProxy(self, i, j)

  def edges = {
    val visited = AutoSet[K]()
    keys flatMap { i: K ⇒
      visited += i
      adjacentEdges(i) filter { e ⇒ visited notContains e.key2 }
    }
  }

  def adjacentKeySet(i: K): Set[K]

  def adjacentMap(i: K) = adjacentKeySet(i) createMapBy { j ⇒ apply(i, j) }
  def adjacentKeys(i: K) = adjacentKeySet(i).elements
  def adjacentNodes(i: K) = adjacentKeys(i) map node
  def adjacentEdges(i: K) = adjacentKeys(i) map { j ⇒ edge(i, j) }

  override def reverse = self

  //TODO: map, zip, ...

}

object UndirectedGraph {

  class EdgeProxy[K, +E](val graph: UndirectedGraph[K, E], val key1: K, val key2: K) extends GraphEdge[K, E] {
    override def equals(that: Any) = that match {
        case that: UndirectedGraph.EdgeProxy[K, E] =>
          (this.graph eq that.graph) &&
          ((this.key1 == that.key1 && this.key2 == that.key2) ||
            (this.key1 == that.key2 && this.key2 == that.key1))
        case _ => false
      }
    def data = graph(key1, key2)
    def node1 = graph.node(key1)
    def node2 = graph.node(key2)
    override def hashCode = poly.algebra.Hashing.byRef.hash(graph) + (key1.## ^ key2.##)
    def contains(x: K) = x == key1 || x == key2
    def keys = ListSeq(key1, key2)
  }
}
