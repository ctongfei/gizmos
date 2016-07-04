package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.mut._

/**
 * Represents a multimap, in which each key can potentially be mapped to multiple values.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Multimap[K, V] extends Relation[K, V] with KeyedLike[K, Multimap[K, V]] with PartialFunction[K, Set[V]] { self =>

  def keys: Iterable[K]

  def pairs: Iterable[(K, V)] = for (k <- keys; v <- apply(k).elements) yield (k, v)

  implicit def eqOnValues: Eq[V]

  def apply(k: K): Set[V]

  def containsKey(x: K): Boolean

  def related(k: K, v: V) = apply(k) contains v

  def keySet: Set[K] = new AbstractSet[K] {
    def eqOnKeys = self.eqOnKeys
    def keys = self.keys
    def contains(x: K) = self.containsKey(x)
  }

  def pairSet: Set[(K, V)] = new AbstractSet[(K, V)] {
    def keys = self.pairs
    def contains(x: (K, V)) = self(x._1) contains x._2
    def eqOnKeys = self.eqOnKeys product self.eqOnValues
  }

  /** $EAGER */
  def valueSet: Set[V] = pairs map second to AutoSet

  def size = pairs.size

  def isDefinedAt(k: K) = containsKey(k)


  // HELPER FUNCTIONS

  def isEmpty = size != 0

  def filterKeys(f: K => Boolean): Multimap[K, V] = new MultimapT.KeyFiltered(self, f)

  def union(that: Multimap[K, V]) = new Multimap[K, V] {
    def keys = self.keys ++ that.keys.filterNot(self.containsKey)
    def eqOnKeys = self.eqOnKeys
    def eqOnValues = self.eqOnValues
    def apply(k: K) = self(k) union that(k)
    def containsKey(k: K) = self.containsKey(k) | that.containsKey(k)
  }

  def product[L, W](that: Multimap[L, W]) = new MultimapT.Product(self, that)

  def map[W](f: Multimap[V, W]): Multimap[K, W] = new MultimapT.Composed(f, self)

  def map[W](f: Bijection[V, W]): Multimap[K, W] = ???

  def contramap[J](f: Bijection[J, K]): Multimap[J, V] = ???

  def contramap[J](that: Multimap[J, K]): Multimap[J, V] = new MultimapT.Composed(self, that)

  def compose[J](that: Multimap[J, K]): Multimap[J, V] = contramap(that)

  def andThen[W](that: Multimap[V, W]): Multimap[K, W] = map(that)

  override def inverse: Multimap[V, K] = ???

  /**
   * Casts this multimap of type `Multimap[K, V]` to the equivalent map of type `Map[K, Set[V]]`.
   */
  def asMap: Map[K, Set[V]] = ???

}

object Multimap {

  implicit object Semicategory
  //TODO: semicategory. does not have id.
  //TODO: update poly.algebra.hkt.Semigroupoid

}

abstract class AbstractMultimap[K, V] extends Multimap[K, V]

private[poly] object MultimapT {

  class KeySet[K](self: Multimap[K, _]) extends Set[K] {
    def eqOnKeys = self.eqOnKeys
    def keys = self.pairs map first
    def contains(x: K) = self containsKey x
  }

  class KeyFiltered[K, V](self: Multimap[K, V], f: K => Boolean) extends Multimap[K, V] {
    override def pairs = self.pairs filter { f compose first }
    implicit def eqOnValues = self.eqOnValues
    def apply(k: K) = if (f(k)) self(k) else Set.empty[V]
    def containsKey(x: K) = self.containsKey(x) && f(x)
    def keys = self.keys filter f
    def eqOnKeys = self.eqOnKeys
  }

  class Product[K, L, V, W](self: Multimap[K, V], that: Multimap[L, W]) extends Multimap[(K, L), (V, W)] {
    override def pairs = for ((k, v) <- self.pairs; (l, w) <- that.pairs) yield (k, l) -> (v, w)
    def eqOnValues = self.eqOnValues product that.eqOnValues
    def apply(k: (K, L)) = self(k._1) product that(k._2)
    def containsKey(k: (K, L)) = self.containsKey(k._1) && that.containsKey(k._2)
    def keys = self.keys monadicProduct that.keys
    def eqOnKeys = self.eqOnKeys product that.eqOnKeys
  }

  class Composed[A, B, C](self: Multimap[B, C], that: Multimap[A, B]) extends Multimap[A, C] {
    def keys = that.keys filter containsKey
    implicit def eqOnKeys = that.eqOnKeys
    implicit def eqOnValues = self.eqOnValues
    override def pairSet = {
      for {
        a <- that.keySet
        b <- that(a)
        c <- self(b)
      } yield (a, c)
    }
    def apply(k: A) = that(k) flatMap self
    def containsKey(k: A) = that(k).elements.flatMap((b: B) => self(b).elements).notEmpty
  }

}
