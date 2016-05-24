package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._

/**
 * Represents a directed graph whose underlying representation is an adjacency list.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyListGraph[@sp(Int) K, V, E] private(private val r: KeyMutableMap[K, AdjacencyListGraph.NodeAdjacency[K, V, E]]) extends Graph[K, V, E] {

  def apply(i: K): V = r(i).data

  def apply(i: K, j: K): E = r(i).succ(j)

  def keySet = r.keySet

  def outgoingMapOf(i: K) = r(i).succ

}

object AdjacencyListGraph extends GraphFactory[AdjacencyListGraph] {

  private[poly] class NodeAdjacency[K: Eq, V, E] {
    var data: V = _
    val succ = ListMap[K, E]()
  }

  implicit def newBuilder[K: Eq, V, E]: GraphBuilder[K, V, E, AdjacencyListGraph[K, V, E]] = new GraphBuilder[K, V, E, AdjacencyListGraph[K, V, E]] {
    private val r = AutoMap[K, NodeAdjacency[K, V, E]]()
    def numNodesHint(n: Int) = {}
    def addEdgeInplace(i: K, j: K, e: E) = {
      if (r notContainsKey i) r += (i, new NodeAdjacency[K, V, E])
      r(i).succ += (j, e)
    }
    def addNodeInplace(i: K, v: V) = {
      if (r notContainsKey i) r += (i, new NodeAdjacency[K, V, E])
      r(i).data = v
    }
    def result = new AdjacencyListGraph(r)
  }
}
