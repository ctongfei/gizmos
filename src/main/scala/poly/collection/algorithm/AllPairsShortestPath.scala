package poly.collection.algorithm

import poly.algebra._
import poly.algebra.functions._
import poly.algebra.ops._
import poly.collection._
import poly.collection.exception._
import poly.collection.mut._
import poly.util.specgroup._

/**
 * Runs the Floyd-Warshall algorithm on a specified graph.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class AllPairsShortestPath[K, E : AdditiveMonoid : BoundedUpperSemilattice]
  (val graph: Graph[K, Any, E]) extends MetricSpace[K, E]
{

  private[this] val max = implicitly[BoundedUpperSemilattice[E]].top

  private[this] val d = HashMap[(K, K), E]()
  private[this] val mid = HashMap[(K, K), K]()

  for (i ← graph.keySet; j ← graph.keySet) {
    if (i == j) d((i, j)) = zero[E]
    if (graph.containsEdge(i, j)) d(i → j) = graph(i, j)
  }

  // Floyd-Warshall algorithm
  for (k ← graph.keySet; i ← graph.keySet; j ← graph.keySet) {
    val dik = d.getOrElse(i → k, max)
    val dkj = d.getOrElse(k → j, max)
    val dij = d.getOrElse(i → j, max)
    val dikj = dik + dkj
    if (dikj < dij) {
      d(i → j) = dikj
      mid(i → j) = k
    }
  }

  def dist(i: K, j: K): E = d.getOrElse(i → j, max)
  def pathBetween(i: K, j: K): Seq[K] = ???
}
