package poly.collection.algorithm

import poly.algebra._
import poly.collection._
import poly.collection.search._

/**
 * Runs the Dijkstra algorithm on a given graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class SingleSourceShortestPath[K, E: OrderedAdditiveGroup](val graph: Graph[K, E], val source: K) {

  private[this] val i = new UniformCostIterator(graph, source)
  i run { _ => }
  private[this] val fringe = i.fringe

  /** Returns the shortest distance from source to the given target. */
  def distanceTo(target: K) = fringe.keyElementMap(target).g

  /** Returns the shortest path from source to the given target. */
  def pathTo(target: K) = fringe.keyElementMap(target).pathToRoot.reverse.map(_.data)

}
