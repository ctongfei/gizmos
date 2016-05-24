package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._

/**
 * @author Tongfei Chen
 */
class AdjacencyListBiGraph[@sp(Int) K: Eq, V, E] extends BiGraph[K, V, E] {

  type VertexInfo = AdjacencyListBiGraph.VertexInfo[K, V, E]

  private val r = AutoMap[K, VertexInfo]()

  def apply(i: K): V = r(i).data

  def apply(i: K, j: K): E = r(i).succ(j)

  //def containsArc(i: K, j: K): Boolean = (for (v ← r ? i; e ← v.succ ? j) yield e).isDefined

  def keySet = r.keySet

  def outgoingMapOf(i: K) = r(i).succ

  def incomingMapOf(i: K) = r(i).pred.createMapBy(j => apply(i, j))

}

object AdjacencyListBiGraph {

  private[poly] class VertexInfo[K: Eq, V, E] {
    var data: V = _
    val pred = ListSet[K]()
    val succ = ListMap[K, E]()
  }

}