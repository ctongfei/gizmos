package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.mut._

/**
 * Basic trait for sets whose elements can be iterated.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Set[@sp(Int) T] extends Predicate[T] with KeyedLike[T, Set[T]] { self =>

  implicit def keyEq: Eq[T]

  /**
    * Returns an iterable sequence of all the elements in this set.
    * The elements returned should be distinct under the equivalence relation of this set.
    */
  def keys: Iterable[T]

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  /** Alias for [[contains]]. */
  @inline final def has(x: T) = contains(x)

  @inline final def containsKey(x: T) = contains(x)

  @inline final def notContains(x: T) = !contains(x)

  /** Tests if an element belongs to this set. */
  def apply(x: T) = contains(x)

  /**
   * Returns an iterable sequence of all the elements in this set.
   * The elements returned should be distinct under the equivalence relation of this set.
   */
  def elements = keys

  override def keySet = this

  /** Returns the number of elements of this set. */
  def size = keys.size

  /** Tests if this set is empty. */
  def isEmpty = size == 0

  /** Tests if this set is not empty. */
  final def notEmpty = !isEmpty

  /**
    * Returns the union of two sets.
    * @example {{{ {1, 2, 3} | {2, 4} == {1, 2, 3, 4} }}}
    */
  def union(that: Set[T]): Set[T] = new AbstractSet[T] {
    def keyEq = self.keyEq
    def contains(x: T) = self.contains(x) || that.contains(x)
    def keys = self.keys ++ that.keys.filter(self.notContains)

  }

  /**
    * Returns the intersection of two sets.
    * @example {{{ {1, 2, 3} & {1, 3, 5} == {1, 3} }}}
    */
  def intersect(that: Set[T]): Set[T] = new AbstractSet[T] {
    def keyEq = self.keyEq
    def contains(x: T) = self.contains(x) && that.contains(x)
    def keys = self.keys.filter(that)
  }

  /**
    * Returns the set difference of two sets.
    * @example {{{ {1, 2, 3} &~ {2, 3, 4} == {1} }}}
    */
  def diff(that: Set[T]): Set[T] = new AbstractSet[T] {
    def keyEq = self.keyEq
    def contains(x: T) = self.contains(x) && that.notContains(x)
    def keys = self.keys.filter(that.notContains)
  }

  /** Returns the symmetric difference of two sets. */
  def symmetricDiff(that: Set[T]): Set[T] = new AbstractSet[T] {
    def keys = self.keys.filter(that.notContains) ++ that.keys.filter(self.notContains)
    def contains(x: T) = self.contains(x) ^ that.contains(x)
    implicit def keyEq = self.keyEq
  }

  /** Tests if this set is a subset of another set. */
  def subsetOf(that: Set[T]): Boolean = this forall that

  /** Tests if this set is a strict subset of another set. */
  def properSubsetOf(that: Set[T]): Boolean = (this forall that) && (that exists !this)

  /** Tests if this set is a strict superset of another set. */
  def supersetOf(that: Set[T]): Boolean = that subsetOf this

  /** Tests if this set is a superset of another set. */
  def properSupersetOf(that: Set[T]): Boolean = that properSubsetOf this

  /** Returns the cartesian product of two sets. */
  def product[U](that: Set[U]): Set[(T, U)] = new SetT.Product(self, that)

  /** Using this set as the key set, construct a map by the given function. */
  def createMap[V](f: T => V): Map[T, V] = new SetT.MapByFunc(self, f)

  def createMapPartially[V](f: PartialFunction[T, V]): Map[T, V] = createMapOptionally(f.lift)

  def createMapOptionally[V](f: T => Option[V]): Map[T, V] = new SetT.MapByOptionalFunc(self, f)

  /**
   * Creates a graph where:
   *  - The elements in this set are the nodes of the graph;
   *  - Whether there is an arc between two nodes is dictated by the given function.
   */
  def createGraph[E](f: (T, T) => Option[E]): Graph[T, E] = new SetT.GraphByOptionalFunc(self, f)

  override def filterKeys(f: T => Boolean): Set[T] = new SetT.KeyFiltered(self, f)

  def filter(f: T => Boolean): Set[T] = filterKeys(f)

  def map[U: Eq](f: T => U): Set[U] = elements map f to AutoSet

  def map[U](f: Bijection[T, U]): Set[U] = new SetT.BijectivelyMapped(self, f)

  def flatMap[U: Eq](f: T => Set[U]): Set[U] =
    elements flatMap { x: T => f(x).elements } to AutoSet

  def zip(that: Set[T]): Set[T] = this intersect that

//  def quotient(coarser: Eq[T]): Set[Set[T]] = new AbstractSet[Set[T]] {
//    private[this] val m = AutoMap[T, KeyMutableSet[T]]()(coarser)
//    def keyEq = Set.Eq[T](self.keyEq)
//    def keys = m.values
//    def contains(x: Set[T]) = {
//      if (x.isEmpty) false else keyEq.eq(x, m(x.elements.head))
//    }
//  }

  //def subsets: Set[Set[T]] = new SetT.PowerSet[T](self)

  /**
   * Wraps each element of this set with a bijective function.
   * {{{
   *   Set[T]              S <=> T         Set[S]
   *    self  . contramap  (  f  )    ==   result
   * }}}
   * @example {{{
   *   {1, 2, 3} contramap {'A' <-> 1, 'B' <-> 2, 'C' <-> 3}
   *   == {'A', 'B', 'C'}
   * }}}
   */
  def contramap[S](f: Bijection[S, T]) = map(f.inverse)

  def foreach[U](f: T => U) = elements foreach f

  def fold[U >: T](z: U)(f: (U, U) => U) = elements.fold(z)(f)

  def foldByMonoid[U >: T : Monoid] = elements.foldByMonoid[U]

  def reduce[U >: T](f: (U, U) => U) = elements reduce f

  def reduceBySemigroup[U >: T : Semigroup] = elements.reduceBySemigroup[U]

  def forall(f: T => Boolean) = elements forall f

  def exists(f: T => Boolean) = elements exists f

  def sum[U >: T : AdditiveCMonoid] = elements.sum[U]

  def max(implicit T: Order[T]) = elements.max

  def min(implicit T: Order[T]) = elements.min

  def minAndMax(implicit T: Order[T]) = elements.minMax

  def argmin[U: Order](f: T => U): T = elements.argmin(f)

  def minBy[U: Order](f: T => U) = argmin(f)

  def argmax[U: Order](f: T => U): T = elements.argmax(f)

  def maxBy[U: Order](f: T => U) = argmax(f)

  def argminWithValue[U: Order](f: T => U) = elements.argminWithValue(f)

  def argmaxWithValue[U: Order](f: T => U) = elements.argmaxWithValue(f)


  /**
   * Casts this set as a multiset in which each element appears exactly once.
   * @tparam R Type of the counts of elements in the multiset, can be `Int`, `Double`, etc.
   */
  def asMultiset[R: OrderedRing]: WeightedSet[T, R] = new SetT.AsWeightedSet(self)

  //Symbolic aliases
  def &(that: Set[T]) = this intersect that
  def |(that: Set[T]) = this union that
  def &~(that: Set[T]) = this diff that
  def ⊂(that: Set[T]) = this properSubsetOf that
  def ⊃(that: Set[T]) = this properSupersetOf that
  def ⊆(that: Set[T]) = this subsetOf that
  def ⊇(that: Set[T]) = this supersetOf that
  def ∩(that: Set[T]) = this & that
  def ∪(that: Set[T]) = this | that
  def ∋(x: T) = this contains x
  def ∌(x: T) = this notContains x

  override def toString = s"{${elements.toString0}}"

  override def equals(that: Any) = that match {
    case that: Set[T] => (this subsetOf that) && (this supersetOf that)
    case _ => false
  }

  override def hashCode = MurmurHash3.symmetricHash(self.elements)(Hashing.default[T])
}

object Set extends Factory1[Id, Set, Eq] {

  // CONSTRUCTORS

  def newBuilder[T: Eq] = AutoSet.newBuilder[T]

  // TYPECLASSES INSTANCES

  /** Returns a new builder of this collection type. */
  implicit def Eq[T](implicit T: Eq[T]): Eq[Set[T]] = T match {
    case th: Hashing[T] => new SetT.SetHashing[T]()(th)
    case _ => new SetT.SetEq[T]
  }

  def Hashing[T: Hashing]: Hashing[Set[T]] = new SetT.SetHashing[T]

  /** Returns the lattice on sets. */
  implicit def Lattice[T]: Lattice[Set[T]] = new SetT.SetLattice[T]

  def ContainmentOrder[T]: PartialOrder[Set[T]] = new SetT.ContainmentOrder[T]

  /** Returns an instance that can compute the Jaccard similarity / distance between two sets. */
  def Jaccard[T] = new SetT.Jaccard[T]

}

abstract class AbstractSet[@sp(Int) T] extends Set[T]

private[poly] object SetT {

  class SetEq[T] extends Eq[Set[T]] {
    def eq(x: Set[T], y: Set[T]) = (x ⊆ y) && (x ⊇ y)
  }

  class SetHashing[T: Hashing] extends SetT.SetEq[T] with Hashing[Set[T]] {
    def hash(x: Set[T]) = MurmurHash3.symmetricHash(x.elements)
  }

  class SetLattice[T] extends Lattice[Set[T]] {
    def inf(x: Set[T], y: Set[T]) = x ∩ y
    def sup(x: Set[T], y: Set[T]) = x ∪ y
  }

  class ContainmentOrder[T] extends PartialOrder[Set[T]] {
    override def eq(x: Set[T], y: Set[T]) = (x ⊆ y) && (x ⊇ y)
    def le(x: Set[T], y: Set[T]) = x ⊆ y
  }

  class KeyFiltered[T](self: Set[T], f: T => Boolean) extends AbstractSet[T] {
    def keyEq = self.keyEq
    def contains(x: T) = self.contains(x) && f(x)
    def keys = self.keys filter f
  }

  class Product[T, U](self: Set[T], that: Set[U]) extends AbstractSet[(T, U)] {
    def keyEq = self.keyEq product that.keyEq
    def keys = self.keys product that.keys
    override def size = self.size * that.size
    def contains(k: (T, U)) = self.containsKey(k._1) && that.containsKey(k._2)
  }

  class MapByFunc[T, U](self: Set[T], f: T => U) extends AbstractMap[T, U] {
    def keySet = self
    def apply(k: T) = f(k)
    def ?(k: T) = if (self contains k) Some(f(k)) else None
    override def size = self.size
  }

  class MapByOptionalFunc[T, U](self: Set[T], f: T => Option[U]) extends AbstractMap[T, U] {
    def keySet = self filter { k => f(k).isDefined }
    def apply(k: T) = f(k).get
    def ?(k: T) = if (self contains k) f(k) else None
    override def pairs = for (k <- self.keys; v <- f(k)) yield (k, v)
  }

  class BijectivelyMapped[T, U](self: Set[T], f: Bijection[T, U]) extends AbstractSet[U] {
    def keyEq = self.keyEq contramap f.invert
    def keys = self.elements map f
    def contains(x: U) = self contains f.invert(x)
  }
/*
  class PowerSet[T](self: Set[T]) extends AbstractSet[Set[T]] {
    implicit def keyEq = new SetT.SetEq[T]
    def keys = ???
    def contains(x: Set[T]) = x subsetOf self
  }
*/
  class GraphByOptionalFunc[T, U](self: Set[T], f: (T, T) => Option[U]) extends AbstractGraph[T, U] {
    def keySet = self.keySet
    def apply(i: T, j: T) = f(i, j).get
    def ?(i: T, j: T) = f(i, j)
    def containsArc(i: T, j: T) = self.contains(i) && self.contains(j) && f(i, j).isDefined
    def outgoingKeySet(i: T) = self filterKeys { j => f(i, j).isDefined }
    override def outgoingMap(i: T) = self createMapOptionally  { j => f(i, j) }
  }

  class AsWeightedSet[T, R: OrderedRing](self: Set[T]) extends AbstractWeightedSet[T, R] {
    def weightRing = OrderedRing[R]
    def weight(k: T) = if (self.contains(k)) one[R] else zero[R]
    def keySet = self.keySet
  }

  class Empty[T: Eq] extends AbstractSet[T] {
    def keyEq = poly.algebra.Eq[T]
    override def size = 0
    def keys = Iterable.Empty
    def contains(x: T) = false
    override def union(that: Set[T]) = that
    override def diff(that: Set[T]) = this
    override def intersect(that: Set[T]) = this
    override def subsetOf(that: Set[T]) = true
    override def properSubsetOf(that: Set[T]) = that.size != 0
  }

  class Jaccard[T] extends Similarity[Set[T], Double] with MetricSpace[Set[T], Double] {
    implicit def similarityOrder = poly.algebra.std.DoubleStructure
    def sim(x: Set[T], y: Set[T]) = {
      val n = x.elements.count(y)
      n / (x.size + y.size - n)
    }
    def dist(x: Set[T], y: Set[T]) = 1.0 - sim(x, y)
  }

}
