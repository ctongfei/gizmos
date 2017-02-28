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

  val g2 = g to AdjacencyListUndirectedGraph

  val g3 = AdjacencyMatrixGraph.from(g.keys, g.arcs)

  g.breadthFirstTraversal(1) foreach println

  println(g)

  val gr = g.reverse

  println(gr)

  val apsp = new AllPairsShortestPath(g2)

  val map = g2.keySet product g2.keySet createMap { case (i, j) => apsp.dist(i, j) }

  apsp.pathBetween(2, 5) foreach println

  val sssp = new SingleSourceShortestPath(g, 1)

  val bp = 0

}
