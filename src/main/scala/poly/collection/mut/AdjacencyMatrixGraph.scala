package poly.collection.mut

import poly.collection._
import poly.collection.impl.specialized._

/**
 * Represents a graph whose underlying representation is an adjacency matrix.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyMatrixGraph[V, E] private(
  private val n: Int,
  private val nodeData: ValueMutableSeq[V],
  private val edgeExists: SpArrayTable[Boolean],
  private val edgeData: ValueMutableTable[E]
) extends AbstractBiGraph[Int, V, E] with ValueMutableGraph[Int, V, E] {

  override def numNodes = n

  def incomingKeysOf(j: Int) = Range(n).filter(i => edgeExists(i, j))

  def outgoingKeysOf(i: Int) = Range(n).filter(j => edgeExists(i, j))

  def containsArc(i: Int, j: Int) = edgeExists(i, j)

  /** Gets the data on the node indexed by the specific key. */
  def apply(i: Int) = nodeData(i)

  /** Gets the data on the edge indexed by the specific two keys. */
  def apply(i: Int, j: Int) = edgeData(i, j)

  def keySet = Range(n).asSet

  def update(i: Int, j: Int, e: E) = {
    edgeExists(i, j) = true
    edgeData(i, j) = e
  }

  def update(i: Int, v: V) = {
    nodeData(i) = v
  }

}
