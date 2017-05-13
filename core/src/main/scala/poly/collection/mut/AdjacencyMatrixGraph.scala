package poly.collection.mut

import poly.collection._
import poly.collection.evidence._
import poly.collection.factory._
import poly.collection.impl._

/**
 * Represents a graph whose underlying representation is an adjacency matrix.
 *
 * The keys of an adjacency matrix graph is restricted to the integer set '''Z''',,''n'',, = {0, 1, ..., ''n''-1}
 * where ''n'' is the number of nodes in this graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyMatrixGraph[E] private[collection](
  override val keySet: BitSet,
  private val edgeData: ResizableTable[E]
) extends AbstractBidiGraph[Int, E] with ValueMutableGraph[Int, E] {

  def incomingKeySet(j: Int) = keySet filter { edgeData(_, j) != null }
  def outgoingKeySet(i: Int) = keySet filter { edgeData(i, _) != null }

  def ?(i: Int, j: Int) = Option(edgeData(i, j))

  override def containsArc(i: Int, j: Int) = edgeData(i, j) != null

  def apply(i: Int, j: Int) = edgeData(i, j)

  def addArc_!(i: Int, j: Int, e: E) = update(i, j, e)

  def removeArc_!(i: Int, j: Int) = edgeData.set(i, j, null)

  def update(i: Int, j: Int, e: E) = edgeData(i, j) = e

  def adjacencyMatrix: Table[Option[E]] = {
    val n = keySet.max + 1
    Table.tabulate(n, n) { (i, j) => Option(edgeData(i, j)) }
  }

}

object AdjacencyMatrixGraph extends GraphFactory[({type λ[α, β] = AdjacencyMatrixGraph[β]})#λ, IsInt] {
  def newGraphBuilder[K: IsInt, E]: GraphBuilder[K, E, AdjacencyMatrixGraph[E]] = new GraphBuilder[Int, E, AdjacencyMatrixGraph[E]] {
    private[this] val keySet = BitSet()
    private[this] val data = new ResizableTable[E]()
    private[this] var n = -1
    def addKey(i: Int) = {
      keySet += i
      data.ensureRowColCapacity(i, i)
    }
    def addArc(i: Int, j: Int, e: E) = {
      keySet += i
      keySet += j
      data(i, j) = e
    }
    def result() = new AdjacencyMatrixGraph[E](keySet, data)
  }.asInstanceOf[GraphBuilder[K, E, AdjacencyMatrixGraph[E]]] // this type cast is safe
}
