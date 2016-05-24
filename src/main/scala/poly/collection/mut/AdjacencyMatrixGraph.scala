package poly.collection.mut

import poly.collection._
import poly.collection.impl.specialized._

/**
 * Represents a graph whose underlying representation is an adjacency matrix.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyMatrixGraph[V, E] private(
  override val numNodes: Int,
  private val nodeData: ValueMutableSeq[V],
  private val edgeExists: SpArrayTable[Boolean],
  private val edgeData: ValueMutableTable[E]
) extends AbstractBiGraph[Int, V, E] with ValueMutableGraph[Int, V, E] {


  def incomingMapOf(j: Int) = Range(numNodes).asSet.createMapByOptional(i => if (edgeExists(i, j)) Some(edgeData(i, j)) else None)

  def outgoingMapOf(i: Int) = Range(numNodes).asSet.createMapByOptional(j => if (edgeExists(i, j)) Some(edgeData(i, j)) else None)

  override def containsArc(i: Int, j: Int) = edgeExists(i, j)

  /** Gets the data on the node indexed by the specific key. */
  def apply(i: Int) = nodeData(i)

  /** Gets the data on the edge indexed by the specific two keys. */
  def apply(i: Int, j: Int) = edgeData(i, j)

  def keySet = Range(numNodes).asSet

  def update(i: Int, j: Int, e: E) = {
    edgeExists(i, j) = true
    edgeData(i, j) = e
  }

  def update(i: Int, v: V) = {
    nodeData(i) = v
  }

  def adjacencyMatrix: Table[Option[E]] = new AbstractTable[Option[E]] {
    def apply(i: Int, j: Int) = if (edgeExists(i, j)) Some(edgeData(i, j)) else None
    def numRows = numNodes
    def numCols = numNodes
  }

}
