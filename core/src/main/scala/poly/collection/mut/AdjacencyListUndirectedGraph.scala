package poly.collection.mut


import poly.collection._
import poly.collection.factory._
import poly.collection.mut.AdjacencyListBidiGraph.VertexInfo
import poly.collection.specgroup._

/**
 * Represents an undirected graph backed by a bidirectional adjacency list storage.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyListUndirectedGraph[@sp(Int) K: Eq, E] private(private val r: KeyMutableMap[K, AdjacencyListBidiGraph.VertexInfo[K, E]])
  extends AdjacencyListBidiGraph(r) with UndirectedGraph[K, E]
{
  def containsEdge(i: K, j: K) = r(i).succ containsKey j
  def adjacentKeySet(i: K) = r(i).pred | r(i).succ.keySet

  override def incomingKeySet(i: K) = adjacentKeySet(i)
  override def outgoingKeySet(i: K) = adjacentKeySet(i)

  override def containsArc(i: K, j: K) = super.containsArc(i, j)

  override def addArc_!(i: K, j: K, e: E) = {
    super.addArc_!(i, j, e)
    super.addArc_!(j, i, e)
  }

  override def removeArc_!(i: K, j: K) = {
    super.removeArc_!(i, j)
    super.removeArc_!(j, i)
  }

  override def update(i: K, j: K, e: E) = {
    super.update(i, j, e)
    super.update(j, i, e)
  }
}

object AdjacencyListUndirectedGraph extends GraphFactory[AdjacencyListUndirectedGraph, Eq] {

  def newGraphBuilder[K: Eq, E]: GraphBuilder[K, E, AdjacencyListUndirectedGraph[K, E]] =
    new GraphBuilder[K, E, AdjacencyListUndirectedGraph[K, E]] {
      private[this] val r = AutoMap[K, VertexInfo[K, E]]().withDefaultUpdate(new VertexInfo)
      def addKey(i: K) = {
        r += i -> new VertexInfo[K, E]
      }
      def addArc(i: K, j: K, e: E) = {
        r(i).succ += (j, e)
        r(i).pred += j
        r(j).succ += (i, e)
        r(j).pred += i
      }
      def result = new AdjacencyListUndirectedGraph(r)
    }

}