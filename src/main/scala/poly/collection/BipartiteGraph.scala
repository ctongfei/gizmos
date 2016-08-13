package poly.collection

import poly.algebra._
import poly.collection.exception._

/**
 * Represents an undirected bipartite graph.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BipartiteGraph[A, B, +E] extends Relation[A, B] with PartialFunction[(A, B), E] { self =>

  def key1Eq: Eq[A] = keySet1.keyEq

  def key2Eq: Eq[B] = keySet2.keyEq

  def keySet1: Set[A]

  def keySet2: Set[B]

  def adjacent(a: A, b: B): Boolean

  def adjacentKeySet1(a: A): Set[B]

  def adjacentKeySet2(b: B): Set[A]

  def apply(a: A, b: B): E

  def apply(ab: (A, B)) = apply(ab._1, ab._2)

  def ?(a: A, b: B): Option[E]

  def isDefinedAt(x: (A, B)) = ??? //TODO: containsEdge

  def map[F](f: E => F): BipartiteGraph[A, B, F]

  override def inverse: BipartiteGraph[B, A, E] = ???

  def asGraph: Graph[Either[A, B], E] = new AbstractGraph[Either[A, B], E] {
    def keySet: Set[Either[A, B]] = new AbstractSet[Either[A, B]] {
      def keyEq = key1Eq coproduct key2Eq
      def keys = self.keySet1.elements.map(Left(_)) ++ self.keySet2.elements.map(Right(_))
      def contains(x: Either[A, B]) = x match {
        case Left(a) => self.keySet1.contains(a)
        case Right(b) => self.keySet2.contains(b)
      }
    }
    def apply(i: Either[A, B], j: Either[A, B]) = (i, j) match {
      case (Left(a), Right(b)) => self.apply(a, b)
      case _ => throw new KeyNotFoundException(i, j)
    }
    def ?(i: Either[A, B], j: Either[A, B]) = (i, j) match {
      case (Left(a), Right(b)) => self ? (a, b)
      case _ => None
    }
    def containsArc(i: Either[A, B], j: Either[A, B]) = (i, j) match {
      case (Left(a), Right(b)) => self.adjacent(a, b)
      case _ => false
    }
    def outgoingKeySet(i: Either[A, B]) = i match {
      case Left(a) => self.adjacentKeySet1(a).map(Bijection[B, Either[A, B]](b => Right(b), r => r.right.get))
      case Right(b) => self.adjacentKeySet2(b).map(Bijection[A, Either[A, B]](a => Left(a), l => l.left.get))
    }
  }

  def asMultimap: BiMultimap[A, B] = new AbstractBiMultimap[A, B] {
    def keySet = self.keySet1
    def valueSet = self.keySet2
    def apply(k: A) = self.adjacentKeySet1(k)
    def invert(b: B) = self.adjacentKeySet2(b)
  }

}

object BipartiteGraph {



}