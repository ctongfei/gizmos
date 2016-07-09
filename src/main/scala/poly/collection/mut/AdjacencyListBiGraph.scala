package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._
import poly.collection.builder._
import poly.collection.factory._

/**
 *
 * @since 0.1.0
 * @author Tongfei Chen
 */
class AdjacencyListBiGraph[@sp(Int) K: Eq, E] private(private val r: KeyMutableMap[K, AdjacencyListBiGraph.VertexInfo[K, E]]) extends BiGraph[K, E] {

  def apply(i: K, j: K): E = r(i).succ(j)

  def ?(i: K, j: K) = for (v <- r ? i; x <- v.succ ? j) yield x

  def incomingKeySet(i: K) = r(i).pred
  def outgoingKeySet(i: K) = r(i).succ.keySet

  def keys = r.keys
  def containsKey(i: K) = r.containsKey(i)
  def containsArc(i: K, j: K) = r.containsKey(i) && r(i).succ.containsKey(j)
  implicit def eqOnKeys = r.eqOnKeys

}

object AdjacencyListBiGraph extends BuilderFactoryAAB_EvA[AdjacencyListBiGraph, Eq] {

  private[poly] class VertexInfo[K: Eq, E] {
    val pred = ListSet[K]()
    val succ = ListMap[K, E]()
  }

  implicit def newBuilder[K: Eq, E]: Builder[(K, K, E), AdjacencyListBiGraph[K, E]] =
    new Builder[(K, K, E), AdjacencyListBiGraph[K, E]] {
      private[this] val r = AutoMap[K, VertexInfo[K, E]]().withDefaultUpdate(new VertexInfo[K, E])
      def addInplace(x: (K, K, E)) = {
        val (i, j, e) = x
        r(i).succ += (j, e)
        r(j).pred += i
      }
      def result = new AdjacencyListBiGraph(r)
    }


}
