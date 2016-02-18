package poly.collection.mut

import poly.collection._
import poly.util.specgroup._

/**
 * @author Tongfei Chen
 */
class AdjacencyListBiGraph[@sp(i) K, V, E] extends BiGraph[K, V, E] {

  private class VertexInfo {
    var data: V = _
    val pred = ListSet[K]()
    val succ = HashMap[K, E]() // TODO: ListMap
  }

  private val r = HashMap[K, VertexInfo]()

  def apply(i: K): V = r(i).data

  def apply(i: K, j: K): E = r(i).succ(j)

  def containsArc(i: K, j: K): Boolean = (for (v ← r ? i; e ← v.succ ? j) yield e).isDefined

  def keySet = r.keySet

  def outgoingKeysOf(i: K): Iterable[K] = r(i).succ.keys

  def incomingKeysOf(i: K): Iterable[K] = r(i).pred.elements

}
