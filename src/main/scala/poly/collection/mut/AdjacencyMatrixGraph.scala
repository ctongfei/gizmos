package poly.collection.mut

import poly.collection._
import poly.collection.impl.specialized._

/**
 * @author Tongfei Chen
 */
abstract class AdjacencyMatrixGraph[V, E] private(
  private val n: Int,
  private val nodeData: Seq[V],
  private val edgeExists: SpArrayTable[Boolean],
  private val edgeData: Table[E]
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

}
