package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl.specialized._

/**
 * Represents a graph whose underlying representation is an adjacency matrix.
 *
 * The keys of an adjacency matrix graph is restricted to the integer set '''Z''',,''n'',, = {0, 1, ..., ''n''-1}
 * where ''n'' is the number of nodes in this graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyMatrixGraph[E] private[collection](
  override val numNodes: Int,
  private val edgeData: ValueMutableTable[E]
) extends AbstractBidiGraph[Int, E] with ValueMutableGraph[Int, E] {

  def incomingKeySet(j: Int) = keySet filter { edgeData(_, j) != null }
  def outgoingKeySet(i: Int) = keySet filter { edgeData(i, _) != null }

  override def keySet = Range(numNodes).asSet

  def ?(i: Int, j: Int) = Option(edgeData(i, j))

  override def containsArc(i: Int, j: Int) = edgeData(i, j) != null

  def apply(i: Int, j: Int) = edgeData(i, j)

  def addArc_!(i: Int, j: Int, e: E) = update(i, j, e)

  def removeArc_!(i: Int, j: Int) = edgeData(i, j) = null.asInstanceOf[E]

  def update(i: Int, j: Int, e: E) = edgeData(i, j) = e

  def adjacencyMatrix: Table[Option[E]] = Table.tabulate(numNodes, numNodes) { (i, j) =>
    Option(edgeData(i, j))
  }

}
