package poly.collection

import poly.algebra._

/**
 * Represents an undirected bipartite graph.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BipartiteGraph[A, B, E] extends Relation[A, B] with PartialFunction[(A, B), E] { self =>

  def eqOnKeys1: Eq[A] = keySet1.eqOnKeys

  def eqOnKeys2: Eq[B] = keySet2.eqOnKeys

  def keySet1: Set[A]

  def keySet2: Set[B]

  def adjacent(a: A, b: B) = ???

  def adjacentKeySet1(a: A): Set[B]

  def adjacentKeySet2(b: B): Set[A]

  def apply(a: A, b: B): E

  def apply(ab: (A, B)) = apply(ab._1, ab._2)

  def isDefinedAt(x: (A, B)) = ??? //TODO: containsEdge

  def map[F](f: E => F): BipartiteGraph[A, B, F]

  override def inverse: BipartiteGraph[B, A, E] = ???

  def asGraph: Graph[Either[A, B], E]

  def asMultimap: Multimap[A, B] = new AbstractMultimap[A, B] { //TODO: BiMultimap
    def eqOnValues = self.eqOnKeys2
    def apply(k: A) = self.adjacentKeySet1(k)
    def containsKey(x: A) = self.keySet1.contains(x)
    def keys = self.keySet1.elements
    def eqOnKeys = self.eqOnKeys1
  }

}

object BipartiteGraph {



}