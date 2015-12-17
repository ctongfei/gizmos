package poly.collection.algorithm

import poly.algebra._
import poly.algebra.function._
import poly.algebra.ops._
import poly.collection._
import poly.collection.mut._

/**
 * Runs the Floyd-Warshall algorithm on a specified graph.
 * @author Tongfei Chen
 */
class AllPairsShortestPath[K, E : OrderedAdditiveGroup : HasTop]
  (val graph: Graph[K, Any, E]) extends MetricSpace[K, E]
{

  private[this] val max = top[E]

  private[this] val d = HashMap[(K, K), E]()
  private[this] val mid = HashMap[(K, K), K]()

  for (i ← graph.keys; j ← graph.keys) {
    if (i == j) d(i → j) = zero[E]
    else if (graph containsEdge (i, j)) d(i → j) = graph(i, j)
  }

  // Floyd-Warshall algorithm
  for (k ← graph.keys; i ← graph.keys; j ← graph.keys) {
    val dik = d ? (i → k)
    val dkj = d ? (k → j)
    val dij = d ? (i → j)
    val dikj = for { ik ← dik; kj ← dkj } yield ik + kj
    if (dikj.isDefined && dij.isDefined && (dij.isEmpty || (dikj.get < dij.get)))
    if (dikj.get < dij.get) {
      d(i → j) = dikj.get
      mid(i → j) = k
    }
  }

  def dist(i: K, j: K): E = d.getOrElse(i → j, max)
  def pathBetween(i: K, j: K): Seq[K] = ???
}
