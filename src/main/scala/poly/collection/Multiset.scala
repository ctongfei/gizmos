package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._

/**
 * A multiset in which elements can appear more than once.
 *
 * The multiplicity in Poly-collection's Multiset can be real-valued:
 * as long as it forms an ordered ring, it can be used as a type for the
 * counts for the elements.
 *
 * Multisets are useful for implementing counters and sparse feature vectors.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Multiset[@sp(Int) K, @sp(Int, Double) R] extends KeyedLike[K, Multiset[K, R]] { self =>

  implicit def equivOnKeys: Equiv[K]

  /** Returns the ring structure endowed on the counts of this multiset. */
  implicit def ringOnCount: OrderedRing[R]

  def contains(k: K): Boolean

  final def containsKey(k: K) = contains(k)

  def multiplicity(k: K): R

  def keys: Iterable[K]

  def pairs: Iterable[(K, R)] = keys.map(k => k → multiplicity(k))

  final def apply(k: K): R = multiplicity(k)

  def asKeyCountMap: Map[K, R] = new AbstractMap[K, R] {
    def pairs = self.pairs
    def containsKey(x: K) = self contains x
    def apply(k: K) = self(k)
    def ?(k: K) = if (self.multiplicity(k) == zero[R]) None else Some(self.multiplicity(k))
    def equivOnKeys = self.equivOnKeys
  }

  def filterKeys(f: K => Boolean): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKeys = self.equivOnKeys
    override def pairs  = self.pairs.filter { case (k, r) => f(k) }
    def multiplicity(k: K) = if (f(k)) self.multiplicity(k) else zero[R]
    implicit def ringOnCount = self.ringOnCount
    def keys = self.keys.filter(f)
    def contains(k: K) = f(k) && self.contains(k)
  }

  def keySet: Set[K] = new AbstractSet[K] {
    def equivOnKeys = self.equivOnKeys
    def keys = self.keys
    def contains(k: K) = self.containsKey(k)
  }

  def scale(w: R): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKeys = self.equivOnKeys
    implicit def ringOnCount = self.ringOnCount
    override def pairs = self.pairs.map { case (k, r) => k → (r * w) }
    def multiplicity(k: K) = self.multiplicity(k) * w
    def keys = self.keys
    def contains(k: K) = self.contains(k)
  }

  def intersect(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def equivOnKeys = self.equivOnKeys
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = function.min(self.multiplicity(k), that.multiplicity(k))
    def keys = self.keys intersect that.keys
    def contains(k: K) = self.contains(k) && that.contains(k)
  }

  def union(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def equivOnKeys = self.equivOnKeys
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = function.max(self.multiplicity(k), that.multiplicity(k))
    def keys = self.keys union that.keys
    def contains(k: K) = self.contains(k) || that.contains(k)
  }

  def multisetDiff(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def equivOnKeys = self.equivOnKeys
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = function.max(zero[R], self.multiplicity(k) - that.multiplicity(k))
    def keys = self.keys filter self.contains
    def contains(k: K) = self.multiplicity(k) > that.multiplicity(k)
  }

  def product[L](that: Multiset[L, R]): Multiset[(K, L), R] = new AbstractMultiset[(K, L), R] {
    def equivOnKeys = Equiv.product(self.equivOnKeys, that.equivOnKeys)
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: (K, L)) = self.multiplicity(k._1) * that.multiplicity(k._2)
    def keys = self.keys product that.keys
    def contains(k: (K, L)) = self.contains(k._1) && that.contains(k._2)
  }

  def multisetAdd(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    implicit def equivOnKeys = self.equivOnKeys
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = self.multiplicity(k) + that.multiplicity(k)
    def keys = self.keys union that.keys
    def contains(k: K) = self.contains(k) || that.contains(k)
  }

  def subsetOf(that: Multiset[K, R]) = this.keys forall { k => this.multiplicity(k) <= that.multiplicity(k) }

  def supersetOf(that: Multiset[K, R]) = that subsetOf this

  def properSubsetOf(that: Multiset[K, R]) = (this subsetOf that) && (that.keys exists { k => that.multiplicity(k) > this.multiplicity(k) } )

  def properSupersetOf(that: Multiset[K, R]) = that properSubsetOf this

  def :*(w: R): Multiset[K, R] = scale(w)
  def *:(w: R): Multiset[K, R] = scale(w)

  // FOLDING

  def sum[L >: K](implicit m: Module[L, R]) = pairs.map { case (k, w) => m.scale(k, w) }.sum(m)

  def forall(f: K => Boolean) = keys forall f
  def exists(f: K => Boolean) = keys exists f
  def max(implicit K: WeakOrder[K]) = keys.max
  def min(implicit K: WeakOrder[K]) = keys.min
  def minAndMax(implicit K: WeakOrder[K]) = keys.minAndMax
  def argmax[L: WeakOrder](f: K => L) = keys.argmax(f)
  def argmin[L: WeakOrder](f: K => L) = keys.argmin(f)
  def minBy[L: WeakOrder](f: K => L) = argmin(f)
  def maxBy[L: WeakOrder](f: K => L) = argmax(f)

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

  override def toString = "{" + pairs.map { case (k, w) => s"$k: $w"}.buildString(", ") + "}"

  override def equals(that: Any) = that match {
    case that: Multiset[K, R] => Multiset.ContainmentOrder[K, R].eq(this, that)
    case _ => false
  }

}

object Multiset {

  def empty[K: Equiv, R: OrderedRing]: Multiset[K, R] = new Multiset[K, R] {
    def equivOnKeys = Equiv[K]
    def multiplicity(k: K) = OrderedRing[R].zero
    def ringOnCount = OrderedRing[R]
    def keys = Iterable.empty
    def contains(k: K) = false
  }

  /** Returns the implicit module structure on multisets. */
  implicit def Module[K: Equiv, R: OrderedRing]: Module[Multiset[K, R], R] = new Module[Multiset[K, R], R] {
    implicit def ringOnScalar = Ring[R]
    def scale(x: Multiset[K, R], k: R) = x scale k
    def add(x: Multiset[K, R], y: Multiset[K, R]) = x multisetAdd y
    def zero = Multiset.empty[K, R]
  }

  implicit def Lattice[K: Equiv, R: OrderedRing]: Lattice[Multiset[K, R]] = new Lattice[Multiset[K, R]] {
    def sup(x: Multiset[K, R], y: Multiset[K, R]) = x union y
    def inf(x: Multiset[K, R], y: Multiset[K, R]) = x intersect y
  }

  implicit def ContainmentOrder[K: Equiv, R: OrderedRing]: PartialOrder[Multiset[K, R]] = new PartialOrder[Multiset[K, R]] {
    def le(x: Multiset[K, R], y: Multiset[K, R]) = x subsetOf y
    override def eq(x: Multiset[K, R], y: Multiset[K, R]) = (x subsetOf y) && (y subsetOf x)
  }

}

abstract class AbstractMultiset[@sp(Int) K, @sp(Int, Double) R] extends Multiset[K, R]
