package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._

/**
 * A multiset in which elements can appear more than once.
 *
 * The weight/multiplicity in Poly-collection's Multiset can be real-valued:
 * as long as it forms an ordered ring ([[poly.algebra.OrderedRing]]), it can be used as a type for the
 * weights for the elements.
 *
 * Multisets are potentially useful for implementing counters and sparse feature vectors.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Multiset[@sp(Int) K, @sp(Int, Double) R] extends KeyedLike[K, Multiset[K, R]] { self =>

  implicit def keyEq: Eq[K]

  /** Returns the ring structure endowed on the counts of this multiset. */
  implicit def weightRing: OrderedRing[R]

  def contains(k: K): Boolean

  final def notContains(k: K) = !contains(k)

  final def containsKey(k: K) = contains(k)

  /** Returns the weight (multiplicity) of a specific element. */
  def weight(k: K): R

  def keys: Iterable[K]

  def keyWeightPairs = keys.map(k => k -> weight(k))

  // HELPER FUNCTIONS

  final def apply(k: K) = weight(k)

  /**
   * Casts this multiset as a map of keys to their corresponding weights.
   * @example {'a': 2, 'b': 3}.asMap == {'a' -> 2, 'b' -> 3}
   */
  def asMap: Map[K, R] = new AbstractMap[K, R] {
    def keySet = self.keySet
    override def pairs = self.keyWeightPairs
    def apply(k: K) = self(k)
    def ?(k: K) = if (self.weight(k) == weightRing.zero) None else Some(self.weight(k))
  }

  def filterKeys(f: K => Boolean): Multiset[K, R] = new AbstractMultiset[K, R] {
    def keyEq = self.keyEq
    override def keyWeightPairs  = self.keyWeightPairs.filter { case (k, r) => f(k) }
    def weight(k: K) = if (f(k)) self.weight(k) else zero[R]
    implicit def weightRing = self.weightRing
    def keys = self.keys.filter(f)
    def contains(k: K) = f(k) && self.contains(k)
  }

  def keySet: Set[K] = new AbstractSet[K] {
    def keyEq = self.keyEq
    def keys = self.keys
    def contains(k: K) = self.containsKey(k)
  }

  def scale(w: R): Multiset[K, R] = new AbstractMultiset[K, R] {
    def keyEq = self.keyEq
    implicit def weightRing = self.weightRing
    override def keyWeightPairs = self.keyWeightPairs.map { case (k, r) => k -> (r * w) }
    def weight(k: K) = self.weight(k) * w
    def keys = self.keys
    def contains(k: K) = self.contains(k)
  }

  def intersect(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def keyEq = self.keyEq
    implicit def weightRing = self.weightRing
    def weight(k: K) = function.min(self.weight(k), that.weight(k))
    def keys = self.keys intersect that.keys
    def contains(k: K) = self.contains(k) && that.contains(k)
  }

  def union(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def keyEq = self.keyEq
    implicit def weightRing = self.weightRing
    def weight(k: K) = function.max(self.weight(k), that.weight(k))
    def keys = self.keys union that.keys
    def contains(k: K) = self.contains(k) || that.contains(k)
  }

  def multisetDiff(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def keyEq = self.keyEq
    implicit def weightRing = self.weightRing
    def weight(k: K) = function.max(zero[R], self.weight(k) - that.weight(k))
    def keys = self.keys filter self.contains
    def contains(k: K) = self.weight(k) > that.weight(k)
  }

  def product[L](that: Multiset[L, R]): Multiset[(K, L), R] = new AbstractMultiset[(K, L), R] {
    def keyEq = Eq.product(self.keyEq, that.keyEq)
    implicit def weightRing = self.weightRing
    def weight(k: (K, L)) = self.weight(k._1) * that.weight(k._2)
    def keys = self.keys monadicProduct that.keys
    def contains(k: (K, L)) = self.contains(k._1) && that.contains(k._2)
  }

  def multisetAdd(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def keyEq = self.keyEq
    implicit def weightRing = self.weightRing
    def weight(k: K) = self.weight(k) + that.weight(k)
    def keys = self.keys union that.keys
    def contains(k: K) = self.contains(k) || that.contains(k)
  }

  def subsetOf(that: Multiset[K, R]) = this.keys forall { k => this.weight(k) <= that.weight(k) }

  def supersetOf(that: Multiset[K, R]) = that subsetOf this

  def properSubsetOf(that: Multiset[K, R]) = (this subsetOf that) && (that.keys exists { k => that.weight(k) > this.weight(k) } )

  def properSupersetOf(that: Multiset[K, R]) = that properSubsetOf this

  def :*(w: R): Multiset[K, R] = scale(w)
  def *:(w: R): Multiset[K, R] = scale(w)

  // FOLDING

  def sum[L >: K](implicit m: Module[L, R]) = keyWeightPairs.map { case (k, w) => m.scale(k, w) }.sum(m)

  def forall(f: K => Boolean) = keys forall f
  def exists(f: K => Boolean) = keys exists f
  def max(implicit K: Order[K]) = keys.max
  def min(implicit K: Order[K]) = keys.min
  def minAndMax(implicit K: Order[K]) = keys.minAndMax
  def argmax[L: Order](f: K => L) = keys.argmax(f)
  def argmin[L: Order](f: K => L) = keys.argmin(f)
  def minBy[L: Order](f: K => L) = argmin(f)
  def maxBy[L: Order](f: K => L) = argmax(f)

  //Symbolic aliases
  def &(that: Multiset[K, R]) = this intersect that
  def |(that: Multiset[K, R]) = this union that
  def &~(that: Multiset[K, R]) = this multisetDiff that
  def ⊂(that: Multiset[K, R]) = this properSubsetOf that
  def ⊃(that: Multiset[K, R]) = this properSupersetOf that
  def ⊆(that: Multiset[K, R]) = this subsetOf that
  def ⊇(that: Multiset[K, R]) = this supersetOf that
  def ∩(that: Multiset[K, R]) = this intersect that
  def ∪(that: Multiset[K, R]) = this union that
  def ⊎(that: Multiset[K, R]) = this multisetAdd that

  override def toString = "{" + keyWeightPairs.map { case (k, w) => s"$k: $w"}.buildString(", ") + "}"

  override def equals(that: Any) = that match {
    case that: Multiset[K, R] => Multiset.ContainmentOrder[K, R].eq(this, that)
    case _ => false
  }

  override def hashCode = asMap.hashCode

}

object Multiset {

  def empty[K: Eq, R: OrderedRing]: Multiset[K, R] = new Multiset[K, R] {
    def keyEq = Eq[K]
    def weight(k: K) = OrderedRing[R].zero
    def weightRing = OrderedRing[R]
    def keys = Iterable.Empty
    def contains(k: K) = false
  }

  /** Returns the implicit module structure on multisets. */
  implicit def Module[K: Eq, R: OrderedRing]: Module[Multiset[K, R], R] = new Module[Multiset[K, R], R] {
    implicit def scalarRing = Ring[R]
    def scale(x: Multiset[K, R], k: R) = x scale k
    def add(x: Multiset[K, R], y: Multiset[K, R]) = x multisetAdd y
    def zero = Multiset.empty[K, R]
  }

  implicit def Lattice[K: Eq, R: OrderedRing]: Lattice[Multiset[K, R]] = new Lattice[Multiset[K, R]] {
    def sup(x: Multiset[K, R], y: Multiset[K, R]) = x union y
    def inf(x: Multiset[K, R], y: Multiset[K, R]) = x intersect y
  }

  implicit def ContainmentOrder[K: Eq, R: OrderedRing]: PartialOrder[Multiset[K, R]] = new PartialOrder[Multiset[K, R]] {
    def le(x: Multiset[K, R], y: Multiset[K, R]) = x subsetOf y
    override def eq(x: Multiset[K, R], y: Multiset[K, R]) = (x subsetOf y) && (y subsetOf x)
  }

}

abstract class AbstractMultiset[@sp(Int) K, @sp(Int, Double) R] extends Multiset[K, R]
