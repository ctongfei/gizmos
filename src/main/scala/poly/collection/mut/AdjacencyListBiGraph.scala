package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._

/**
 * @author Tongfei Chen
 */
class AdjacencyListBiGraph[@sp(Int) K: Eq, E] private(private val r: KeyMutableMap[K, AdjacencyListBiGraph.VertexInfo[K, E]]) extends BiGraph[K, E] {

  def apply(i: K, j: K): E = r(i).succ(j)
  def incomingKeySet(i: K) = r(i).pred

  def keys = r.keys
  def containsKey(i: K) = r.containsKey(i)
  def containsArc(i: K, j: K) = r.containsKey(i) && r(i).succ.containsKey(j)
  implicit def eqOnKeys = r.eqOnKeys
  def outgoingKeySet(i: K) = r(i).succ.keySet
}

object AdjacencyListBiGraph {

  private[poly] class VertexInfo[K: Eq, E] {
    val pred = ListSet[K]()
    val succ = ListMap[K, E]()
  }

}
