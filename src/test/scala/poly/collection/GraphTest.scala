package poly.collection

import poly.algebra.syntax._
import poly.collection.algorithm._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */

object GraphTest extends App {

  val g = AdjacencyListGraph(
    (1, 2, 1),
    (2, 3, 2),
    (3, 4, 3),
    (4, 5, 4),
    (5, 2, 1),
    (1, 5, 5)
  )

  g.breadthFirstTraversal(1) foreach println

  val apsp = new AllPairsShortestPath(g)

  val bp = 0

}
