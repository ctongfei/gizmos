package poly.collection

import poly.algebra.syntax._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object GraphTest extends App {

  val g = AdjacencyListGraph.withoutNodeData((0, 1, 1), (3, 2, 2), (1, 3, 1), (3, 0, 5), (2, 0, 2))

  g.breadthFirstSearch(0, 2) foreach println

  val bp = 0

}
