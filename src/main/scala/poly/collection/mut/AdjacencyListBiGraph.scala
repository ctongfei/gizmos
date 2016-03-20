package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._

/**
 * @author Tongfei Chen
 */
class AdjacencyListBiGraph[@sp() K: IntHashing, V, E] extends BiGraph[K, V, E] {

  type VertexInfo = AdjacencyListBiGraph.VertexInfo[K, V, E]

  private val r = HashMap[K, VertexInfo]()

  def apply(i: K): V = r(i).data

  def apply(i: K, j: K): E = r(i).succ(j)

  def containsArc(i: K, j: K): Boolean = (for (v ← r ? i; e ← v.succ ? j) yield e).isDefined

  def keySet = r.keySet

  def outgoingKeysOf(i: K): Iterable[K] = r(i).succ.keys

  def incomingKeysOf(i: K): Iterable[K] = r(i).pred.elements

}

object AdjacencyListBiGraph {


  private[poly] class VertexInfo[K, V, E] {
    var data: V = _
    val pred = ListSet[K]()
    val succ = HashMap[K, E]() // should be ListMap
  }

}