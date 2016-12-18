package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._
import poly.collection.factory._

/**
 * @since 0.1.0
 * @author Tongfei Chen
 */
class AdjacencyListBidiGraph[@sp(Int) K: Eq, E] private[collection](private val r: KeyMutableMap[K, AdjacencyListBidiGraph.VertexInfo[K, E]])
  extends AbstractBidiGraph[K, E] with KeyMutableGraph[K, E]
{
  import AdjacencyListBidiGraph._

  def apply(i: K, j: K): E = r(i).succ(j)

  def ?(i: K, j: K) = for (v <- r ? i; x <- v.succ ? j) yield x

  def incomingKeySet(i: K): Set[K] = r(i).pred
  def outgoingKeySet(i: K): Set[K] = r(i).succ.keySet

  def keySet = r.keySet
  def containsArc(i: K, j: K) = r.containsKey(i) && r(i).succ.containsKey(j)

  def addNode_!(i: K) = r += i -> new VertexInfo

  def removeNode_!(i: K) = {
    for (k <- r(i).pred) r(k).succ -= i
    r -= i
  }

  def addArc_!(i: K, j: K, e: E) = {
    r(i).succ += j -> e
    r(j).pred += i
  }

  def removeArc_!(i: K, j: K) = {
    r(i).succ -= j
    r(j).pred -= i
  }

  def update(i: K, j: K, e: E) = {
    r(i).succ(j) = e
  }

}

object AdjacencyListBidiGraph extends GraphFactory[AdjacencyListBidiGraph] {

  private[poly] class VertexInfo[K: Eq, E] {
    val pred = ListSet[K]()
    val succ = ListMap[K, E]()
    override def toString = s"(pred=$pred; succ=$succ)"
  }

  implicit def newBuilder[K: Eq, E]: GraphBuilder[K, E, AdjacencyListBidiGraph[K, E]] =
    new GraphBuilder[K, E, AdjacencyListBidiGraph[K, E]] {
      private[this] val r = AutoMap[K, VertexInfo[K, E]]().withDefaultUpdate(new VertexInfo)
      def addKey(i: K) = {
        r += i -> new VertexInfo[K, E]
      }
      def addArc(i: K, j: K, e: E) = {
        r(i).succ += (j, e)
        r(j).pred += i
      }
      def result = new AdjacencyListBidiGraph(r)
    }
  
}
