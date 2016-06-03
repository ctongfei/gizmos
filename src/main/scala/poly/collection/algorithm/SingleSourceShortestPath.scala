package poly.collection.algorithm

import poly.algebra._
import poly.collection._
import poly.collection.search._

/**
 * Runs the Dijkstra algorithm on a given graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class SingleSourceShortestPath[K, E : OrderedAdditiveGroup : HasTop](val graph: Graph[K, E], start: K) {

  val i = new UniformCostIterator(graph, start)
  while (i.advance()) {}
  val fringe = i.fringe

  def distanceTo(target: K) = fringe.keyElementMap(target).g

  def pathTo(target: K) = fringe.keyElementMap(target).pathToRoot.reverse.map(_.data)

}
