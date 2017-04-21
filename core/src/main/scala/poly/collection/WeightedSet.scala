package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._

/**
 * A weighted set (aka multiset) in which elements can appear more than once.
 *
 * The weight/multiplicity in Poly-collection's `WeightedSet` can be real-valued
 * (as opposed to various `Multiset` implementations similar to the ones in C++ / Guava / Apache Commons Collections):
 * as long as it forms an ordered ring ([[poly.algebra.OrderedRing]]), it can be used as a type for the
 * weights for the elements.
 *
 * Weighted sets are potentially useful for implementing counters and sparse feature vectors.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait WeightedSet[@sp(Int) K, @sp(Int, Double) R] extends KeyedLike[K, WeightedSet[K, R]] { self =>

  def keySet: Set[K]

  /** Returns the ring structure endowed on the counts of this multiset. */
  implicit def weightRing: OrderedRing[R]

  implicit def keyEq: Eq[K] = keySet.keyEq

  def contains(k: K) = keySet contains k

  final def notContains(k: K) = !contains(k)

  final def containsKey(k: K) = contains(k)

  /** Returns the weight (multiplicity) of a specific element. */
  def weight(k: K): R

  def keys = keySet.keys

  def keyWeightPairs = keys.map(k => k -> weight(k))

  // HELPER FUNCTIONS

  final def apply(k: K) = weight(k)

  /**
   * Casts this multiset as a map of keys to their corresponding weights.
   * @example {'a': 2, 'b': 3}.asMap == {'a' -> 2, 'b' -> 3}
   */
  def asMap: Map[K, R] = new WeightedSetT.AsMap(self)

  def filterKeys(f: K => Boolean): WeightedSet[K, R] = new WeightedSetT.KeyFiltered(self, f)

  def scale(w: R): WeightedSet[K, R] = new WeightedSetT.Scaled(self, w)

  def intersect(that: WeightedSet[K, R]): WeightedSet[K, R] = new WeightedSetT.Intersection(self, that)

  def union(that: WeightedSet[K, R]): WeightedSet[K, R] = new AbstractWeightedSet[K, R] {
    implicit def weightRing = self.weightRing
    def weight(k: K) = function.max(self.weight(k), that.weight(k))
    def keySet = self.keySet union that.keySet
  }

  def weightedSetDiff(that: WeightedSet[K, R]): WeightedSet[K, R] = new AbstractWeightedSet[K, R] {
    implicit def weightRing = self.weightRing
    def weight(k: K) = function.max(zero[R], self.weight(k) - that.weight(k))
    def keySet = self.keySet filter (x => self.weight(x) > that.weight(x))
  }

  def product[L](that: WeightedSet[L, R]): WeightedSet[(K, L), R] = new AbstractWeightedSet[(K, L), R] {
    implicit def weightRing = self.weightRing
    def weight(k: (K, L)) = self.weight(k._1) * that.weight(k._2)
    def keySet = self.keySet product that.keySet
  }

  def weightedSetAdd(that: WeightedSet[K, R]): WeightedSet[K, R] = new AbstractWeightedSet[K, R] {
    implicit def weightRing = self.weightRing
    def weight(k: K) = self.weight(k) + that.weight(k)
    def keySet = self.keySet union that.keySet
  }

  def subsetOf(that: WeightedSet[K, R]) = this.keys forall { k => this.weight(k) <= that.weight(k) }

  def supersetOf(that: WeightedSet[K, R]) = that subsetOf this

  def properSubsetOf(that: WeightedSet[K, R]) = (this subsetOf that) && (that.keys exists { k => that.weight(k) > this.weight(k) } )

  def properSupersetOf(that: WeightedSet[K, R]) = that properSubsetOf this

  def :*(w: R): WeightedSet[K, R] = scale(w)
  def *:(w: R): WeightedSet[K, R] = scale(w)

  // FOLDING

  def sum[L >: K](implicit L: Module[L, R]) = keyWeightPairs.map { case (k, w) => L.scale(k, w) }.sum(L)

  def forall(f: K => Boolean) = keys forall f
  def exists(f: K => Boolean) = keys exists f
  def max(implicit K: Order[K]) = keys.max
  def min(implicit K: Order[K]) = keys.min
  def minMax(implicit K: Order[K]) = keys.minMax
  def argmax[L: Order](f: K => L) = keys.argmax(f)
  def argmin[L: Order](f: K => L) = keys.argmin(f)
  def minBy[L: Order](f: K => L) = argmin(f)
  def maxBy[L: Order](f: K => L) = argmax(f)

  //Symbolic aliases
  def &(that: WeightedSet[K, R]) = this intersect that
  def |(that: WeightedSet[K, R]) = this union that
  def &~(that: WeightedSet[K, R]) = this weightedSetDiff that
  def ⊂(that: WeightedSet[K, R]) = this properSubsetOf that
  def ⊃(that: WeightedSet[K, R]) = this properSupersetOf that
  def ⊆(that: WeightedSet[K, R]) = this subsetOf that
  def ⊇(that: WeightedSet[K, R]) = this supersetOf that
  def ∩(that: WeightedSet[K, R]) = this intersect that
  def ∪(that: WeightedSet[K, R]) = this union that
  def ⊎(that: WeightedSet[K, R]) = this weightedSetAdd that

  override def toString = "{" + keyWeightPairs.map { case (k, w) => s"$k: $w"}.buildString(", ") + "}"

  override def equals(that: Any) = that match {
    case that: WeightedSet[K, R] => WeightedSet.ContainmentOrder[K, R].eq(this, that)
    case _ => false
  }

  override def hashCode = asMap.hashCode

}

object WeightedSet {

  /** Creates an empty multiset. */
  def empty[K: Eq, R: OrderedRing]: WeightedSet[K, R] = new WeightedSetT.Empty[K, R]

  implicit class IntWeightedSetOps[K](val ws: WeightedSet[K, Int]) extends AnyVal {

    /**
     * Returns all elements of an Int-weighted set. Elements appearing multiple times
     * are repeated.
     * @example {{{
     *   {1: 2, 2: 3}.elements == (1, 1, 2, 2, 2)
     * }}}
     */
    def elements = ws.keys flatMap { k: K => Iterable.repeat(ws(k))(k) }

  }

  /** Returns the implicit module structure on multisets. */
  implicit def Module[K: Eq, R: OrderedRing]: Module[WeightedSet[K, R], R] = new Module[WeightedSet[K, R], R] {
    implicit def scalarRing = Ring[R]
    def scale(x: WeightedSet[K, R], k: R) = x scale k
    def add(x: WeightedSet[K, R], y: WeightedSet[K, R]) = x weightedSetAdd y
    def zero = WeightedSet.empty[K, R]
  }

  implicit def Lattice[K: Eq, R: OrderedRing]: Lattice[WeightedSet[K, R]] = new Lattice[WeightedSet[K, R]] {
    def sup(x: WeightedSet[K, R], y: WeightedSet[K, R]) = x union y
    def inf(x: WeightedSet[K, R], y: WeightedSet[K, R]) = x intersect y
  }

  implicit def ContainmentOrder[K: Eq, R: OrderedRing]: PartialOrder[WeightedSet[K, R]] = new PartialOrder[WeightedSet[K, R]] {
    def le(x: WeightedSet[K, R], y: WeightedSet[K, R]) = x subsetOf y
    override def eq(x: WeightedSet[K, R], y: WeightedSet[K, R]) = (x subsetOf y) && (y subsetOf x)
  }

}

abstract class AbstractWeightedSet[@sp(Int) K, @sp(Int, Double) R] extends WeightedSet[K, R]

private[poly] object WeightedSetT {

  class Empty[K, R](implicit K: Eq[K], R: OrderedRing[R]) extends WeightedSet[K, R] {
    def weight(k: K) = R.zero
    def weightRing = R
    def keySet = Set.empty[K]
  }

  class AsMap[K, R](self: WeightedSet[K, R]) extends AbstractMap[K, R] {
    def keySet = self.keySet
    override def pairs = self.keyWeightPairs
    def apply(k: K) = self(k)
    def ?(k: K) = if (self.weight(k) == self.weightRing.zero) None else Some(self.weight(k))
  }

  class KeyFiltered[K, R](self: WeightedSet[K, R], f: K => Boolean) extends AbstractWeightedSet[K, R] {
    override def keyWeightPairs  = self.keyWeightPairs.filter { case (k, r) => f(k) }
    def weight(k: K) = if (f(k)) self.weight(k) else zero[R]
    implicit def weightRing = self.weightRing
    def keySet = self.keySet filter f
  }

  class Scaled[K, R](self: WeightedSet[K, R], w: R) extends AbstractWeightedSet[K, R] {
    implicit def weightRing = self.weightRing
    override def keyWeightPairs = self.keyWeightPairs.map { case (k, r) => k -> (r * w) }
    def weight(k: K) = self.weight(k) * w
    def keySet = self.keySet
  }

  class Intersection[K, R](self: WeightedSet[K, R], that: WeightedSet[K, R]) extends AbstractWeightedSet[K, R] {
    implicit def weightRing = self.weightRing
    def weight(k: K) = function.min(self.weight(k), that.weight(k))
    def keySet = self.keySet filter { k => weight(k) > self.weightRing.zero }
  }

}
