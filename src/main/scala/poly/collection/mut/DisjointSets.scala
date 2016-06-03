package poly.collection.mut

import poly.algebra._
import poly.collection.conversion.FromScala._
import poly.collection.node._

/**
 * A union-find disjoint sets structure.
 * This structure keeps track of a set of elements partitioned into a number of disjoint subsets,
 * and enables the user to join two sets at any time.
 *
 * This structure is essential in the Kruskal's algorithm ([[poly.collection.algorithm.MinimumSpanningTree]]).
 * @author Tongfei Chen
 * @since 0.1.0
 */
class DisjointSets[T] private(private val data: KeyMutableMap[T, DisjointSets.Node]) extends Eq[T] {

  import DisjointSets._

  private var numSets = data.size

  /** Returns the number of element this disjoint-sets structure is managing. */
  def size = data.size

  /** Returns the number of distinct disjoint sets in this structure. */
  def numPartitions = numSets

  /** Links two node to form one set using union by rank. */
  private[this] def link(x: Node, y: Node): Unit = {
    if (x.rank > y.rank) y.parent = x
    else {
      x.parent = y
      if (x.rank == y.rank) y.rank += 1
    }
  }

  /** Finds the representative element of a set by path compression. */
  private[this] def find(x: Node): Node = {
    if (x != x.parent) x.parent = find(x.parent)
    x.parent
  }

  /** Joins the two sets in which the two specified elements resides. */
  def join(x: T, y: T): Unit = {
    if (data.eqOnKeys.eq(x, y)) return
    link(find(data(x)), find(data(y)))
    numSets -= 1
  }

  /** Tests if the two specified elements belong to the same set. */
  def eq(x: T, y: T) = {
    if (data.eqOnKeys.eq(x, y)) true
    else find(data(x)) == find(data(y))
  }

}


object DisjointSets {

  private class Node extends NodeWithParent[Nothing] {
    var rank: Int = 0
    var parent: Node = this
    def data = throw new NoSuchElementException
    def isDummy = false
  }

  def apply[T: Eq](xs: T*): DisjointSets[T] =
    new DisjointSets[T](AutoMap from xs.map(t => t -> new Node()))

}
