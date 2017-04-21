package poly.collection.mut

import poly.collection._
import poly.collection.impl._
/**
 * @author Tongfei Chen
 */
class AdjacencyMatrixUndirectedGraph[E] private(keySet: BitSet, edgeData: ResizableTable[E])
  extends AdjacencyMatrixGraph(keySet, edgeData) with ValueMutableGraph[Int, E] with UndirectedGraph[Int, E]
{

  override def containsArc(i: Int, j: Int) = super[AdjacencyMatrixGraph].containsArc(i, j)
  override def incomingKeySet(i: Int) = super[AdjacencyMatrixGraph].incomingKeySet(i)
  override def outgoingKeySet(i: Int) = super[AdjacencyMatrixGraph].outgoingKeySet(i)


  def containsEdge(i: Int, j: Int) = edgeData(i, j) != null

  def adjacentKeySet(i: Int) = outgoingKeySet(i)

  override def addArc_!(i: Int, j: Int, e: E) = {
    super.addArc_!(i, j, e)
    super.addArc_!(j, i, e)
  }

  override def removeArc_!(i: Int, j: Int) = {
    super.removeArc_!(i, j)
    super.removeArc_!(j, i)
  }

  override def update(i: Int, j: Int, e: E) = {
    super.update(i, j, e)
    super.update(j, i, e)
  }

}
