package poly.collection.mut

import poly.algebra._
import poly.collection.node._

/**
 * A union-find disjoint sets structure.
 * This structure keeps track of a set of elements partitioned
 * into a number of disjoint subsets.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class DisjointSets[T] private() extends Eq[T] {

  private class Node extends SinglePredNode[Unit] {
    var rank: Int = 0
    var parent: Node = this
    val data = ()
  }

  private var sets = 0
  private var data = HashMap[T, Node]()

  def size = data.size

  def numPartitions = sets

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
  def union(x: T, y: T): Unit = {
    if (x == y) return
    link(find(data(x)), find(data(y)))
    sets -= 1
  }

  /** Tests if the two specified elements belong to the same set. */
  def eq(x: T, y: T) = {
    if (x == y) true
    else find(data(x)) eq find(data(y))
  }

}


object DisjointSets {

  def apply[T](xs: T*): DisjointSets[T] = {
    val ds = new DisjointSets[T]
    ds.data = HashMap(xs.map(t => t → new ds.Node()): _*)
    ds.sets = ds.data.size
    ds
  }

}
