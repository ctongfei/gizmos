package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.factory._
import poly.collection.mut._

/**
 * Represents a multimap, in which each key can potentially be mapped to multiple values.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Multimap[K, V] extends KeyedLike[K, Multimap[K, V]] with PartialFunction[K, Set[V]] { self =>

  def keySet: Set[K]

  def keys: Iterable[K] = keySet.keys

  /** Returns all key-value pairs in this multimap. */
  def pairs: Iterable[(K, V)] = for (k <- keys; v <- apply(k).elements) yield (k, v)

  implicit def keyEq: Eq[K] = keySet.keyEq

  /** Returns the equivalence relation on values. */
  implicit def valueEq: Eq[V]

  /** Returns the equivalence relation on key-value pairs. */
  implicit def pairEq = keyEq product valueEq

  /** Returns all values that are associated with the given key. */
  def apply(k: K): Set[V]

  def apply(k: K, v: V): Boolean = apply(k) contains v

  /** Checks if a specific key is present in this multimap. */
  def containsKey(x: K) = keySet contains x

  /** Returns a set that contains all key-value pairs in this multimap. */
  def pairSet: Set[(K, V)] = new AbstractSet[(K, V)] {
    def keys = self.pairs
    def contains(x: (K, V)) = self(x._1) contains x._2
    def keyEq = self.keyEq product self.valueEq
  }

  /** Returns the number of distinct (key, value) pairs in this multimap. */
  def size = pairs.size

  def isDefinedAt(k: K) = containsKey(k)

  // HELPER FUNCTIONS

  def isEmpty = size != 0

  def filterKeys(f: K => Boolean): Multimap[K, V] = new MultimapT.KeyFiltered(self, f)

  def union(that: Multimap[K, V]): Multimap[K, V] = new MultimapT.Union(self, that)

  def intersect(that: Multimap[K, V]): Multimap[K, V] = new MultimapT.Intersection(self, that)

  def product[L, W](that: Multimap[L, W]): Multimap[(K, L), (V, W)] = new MultimapT.Product(self, that)

  def map[W](f: Multimap[V, W]): Multimap[K, W] = new MultimapT.Composed(f, self)

  def map[W](f: Bijection[V, W]): Multimap[K, W] = new MultimapT.BijectivelyMapped(self, f)

  def contramap[J](f: Bijection[J, K]): Multimap[J, V] = new MultimapT.BijectivelyContramapped(self, f)

  def contramap[J](that: Multimap[J, K]): Multimap[J, V] = new MultimapT.Composed(self, that)

  def compose[J](that: Multimap[J, K]): Multimap[J, V] = contramap(that)

  def andThen[W](that: Multimap[V, W]): Multimap[K, W] = map(that)

  def inverse: Multimap[V, K] = pairs map { _.swap } to AutoMultimap

  /**
   * Casts this multimap of type `Multimap[K, V]` to the equivalent map of type `Map[ K , Set[V] ]`.
   */
  def asMap: Map[K, Set[V]] = new MultimapT.AsMap(self)

}

object Multimap extends Factory2[Tuple2, Multimap, Eq, Eq] {

  def newBuilder[A: Eq, B: Eq] = AutoMultimap.newBuilder

  //implicit object Semicategory
  //TODO: semicategory. does not have id.
  //TODO: update poly.algebra.hkt.Semigroupoid

}

abstract class AbstractMultimap[K, V] extends Multimap[K, V]

private[poly] object MultimapT {

  class KeySet[K](self: Multimap[K, _]) extends AbstractSet[K] {
    def keyEq = self.keyEq
    def keys = self.pairs map first
    def contains(x: K) = self containsKey x
  }

  class KeyFiltered[K, V](self: Multimap[K, V], f: K => Boolean) extends AbstractMultimap[K, V] {
    override def pairs = self.pairs filter { x => f(x._1) }
    implicit def valueEq = self.valueEq
    def apply(k: K) = if (f(k)) self(k) else Set.empty[V]
    def keySet = self.keySet filter f
  }

  class Product[K, L, V, W](self: Multimap[K, V], that: Multimap[L, W]) extends AbstractMultimap[(K, L), (V, W)] {
    override def pairs = for ((k, v) <- self.pairs; (l, w) <- that.pairs) yield (k, l) -> (v, w)
    def valueEq = self.valueEq product that.valueEq
    def apply(k: (K, L)) = self(k._1) product that(k._2)
    def keySet = self.keySet product that.keySet
  }

  class Composed[A, B, C](self: Multimap[B, C], that: Multimap[A, B]) extends AbstractMultimap[A, C] {
    def keySet = new AbstractSet[A] {
      def keys = that.keys filter containsKey
      implicit def keyEq = that.keyEq
      def contains(k: A) = that(k).elements.flatMap((b: B) => self(b).elements).notEmpty
    }
    implicit def valueEq = self.valueEq
    override def pairSet = {
      for {
        a <- that.keySet
        b <- that(a)
        c <- self(b)
      } yield (a, c)
    }
    def apply(k: A) = that(k) flatMap self
  }

  class BijectivelyContramapped[K, V, J](self: Multimap[K, V], f: Bijection[J, K]) extends AbstractMultimap[J, V] {
    def keySet = self.keySet contramap f
    def valueEq = self.valueEq
    def apply(j: J) = self(f(j))
  }

  class BijectivelyMapped[K, V, W](self: Multimap[K, V], f: Bijection[V, W]) extends AbstractMultimap[K, W] {
    def keySet = self.keySet
    def valueEq = self.valueEq contramap f.invert
    def apply(k: K) = self(k) map f
  }

  class Union[K, V](self: Multimap[K, V], that: Multimap[K, V]) extends AbstractMultimap[K, V] {
    def keySet = self.keySet union that.keySet
    def valueEq = self.valueEq
    def apply(k: K) = self(k) union that(k)
  }

  class Intersection[K, V](self: Multimap[K, V], that: Multimap[K, V]) extends AbstractMultimap[K, V] {
    def keySet = (self.keySet intersect that.keySet) filter { k => (self(k) intersect that(k)).notEmpty }
    def valueEq = self.valueEq
    def apply(k: K) = self(k) intersect that(k)
    override def pairs = (self.pairs intersect that.pairs)
  }

  class AsMap[K, V](self: Multimap[K, V]) extends AbstractMap[K, Set[V]] {
    def keySet = self.keySet
    def ?(k: K) = if (self containsKey k) Some(self(k)) else None
    def apply(k: K) = self(k)
  }

}
