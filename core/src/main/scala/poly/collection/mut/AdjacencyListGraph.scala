package poly.collection.mut

import poly.algebra._
import poly.algebra.specgroup._
import poly.collection._
import poly.collection.factory._

/**
 * Represents a directed graph whose underlying representation is an adjacency list.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class AdjacencyListGraph[@sp(Int) K, E] private(private val r: KeyMutableMap[K, ListMap[K, E]]) extends KeyMutableGraph[K, E] {

  def apply(i: K, j: K): E = r(i)(j)
  def ?(i: K, j: K) = for (x <- r ? i; y <- x ? j) yield y
  def containsArc(i: K, j: K) = r.containsKey(i) && r(i).containsKey(j)
  def keySet = r.keySet
  def outgoingKeySet(i: K) = r(i).keySet

  def addNode_!(i: K) = r += i -> ListMap()

  def removeNode_!(i: K) = {
    for (k <- r.keys) r(k) -= i
    r -= i
  }

  def addArc_!(i: K, j: K, e: E) = {
    if (notContainsKey(i)) addNode_!(i)
    if (notContainsKey(j)) addNode_!(j)
    r(i) += j -> e
  }

  def removeArc_!(i: K, j: K) = r(i) -= j

  def update(i: K, j: K, e: E) = r(i)(j) = e
}

object AdjacencyListGraph extends GraphFactory[AdjacencyListGraph, Eq] {

  def newGraphBuilder[K: Eq, E]: GraphBuilder[K, E, AdjacencyListGraph[K, E]] =
    new GraphBuilder[K, E, AdjacencyListGraph[K, E]] {
      private[this] val r = AutoMap[K, ListMap[K, E]]().withDefaultUpdate(ListMap[K, E]())
      def addKey(i: K) = r += i -> ListMap()
      def addArc(i: K, j: K, e: E) = r(i) += j -> e
      def result = new AdjacencyListGraph(r)
    }

}
