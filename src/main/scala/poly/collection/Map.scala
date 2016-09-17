package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.hkt._
import poly.algebra.specgroup._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.mut._
import scala.language.reflectiveCalls

/**
 * The base trait for maps.
 * A map is a function that maps a key type (domain) to a value type (codomain).
 * In addition to a normal function, a map is also endowed with an iterable set on the type of keys.
 * It can also be viewed as a collection of (key, value) pairs, in which each key is unique under
 * the equivalence relation of the key set.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Map[@sp(Int) K, +V] extends KeyedLike[K, Map[K, V]] with PartialFunction[K, V] with Func[K, V] { self =>

  /** Returns the set of the keys in this map. $LAZY */
  def keySet: Set[K]

  /**
   * Optionally retrieves the value associated with the specified key.
   * @param k The given key
   * @return The associated value. If the key is not found, [[None]] will be returned.
   */
  def ?(k: K): Option[V]

  /**
   * Retrieves the value associated with the specified key.
   * If the key is not found, its behavior is undefined
   * (may or may not throw an exception. This is a deliberate design for efficiency).
   * For maximum safety, use [[?]] to optionally access an element.
   * @param k The given key
   * @return The associated value of ''k''
   * @throws KeyNotFoundException if key not found (may or may not throw)
   */
  def apply(k: K): V

  /** Returns the number of key-value pairs that this map contains. */
  def size: Int = keys.size

  /** Returns an iterable collection of the keys in this map. $LAZY */
  def keys = keySet.keys

  def keyEq = keySet.keyEq

  /** Checks if the specified key is present in this map. */
  def containsKey(x: K) = keySet contains x

  /**
   * Returns the value associated with the given key,
   * or a default value if the key is not present in the map.
   */
  def getOrElse[W >: V](x: K, default: => W) = ?(x) getOrElse default

  final def isDefinedAt(x: K) = containsKey(x)

  /** Returns all key-value pairs stored in this map.
   * @note For performance reasons, this method should be overridden. */
  def pairs: Iterable[(K, V)] = keys map { k => (k, apply(k)) }

  /** Returns all values associated with each key in this map. $LAZY */
  def values = keys map apply

  // HELPER FUNCTIONS

  def isEmpty = size != 0

  def filterKeys(f: K => Boolean): Map[K, V] = new MapT.KeyFiltered(self, f)

  /**
   * Transforms the values of this map according to the specified function.
   * This is the functor operation on maps. $LAZY
   * {{{
   *   K => V        V => W          K => W
   *    self  . map ( that )    ==   result
   * }}}
   * @note This function is equivalent to the Scala library's `mapValues`.
   *       To transform all pairs in this map, use `this.pairs.map`.
   * @example {{{ {1 -> 2, 2 -> 3} map {_ * 2} == {1 -> 4, 2 -> 6} }}}
   * @param f The specific function
   * @return A map view that maps every key of this map to `f(this(key))`.
   */
  override def map[W](f: V => W): Map[K, W] = new MapT.Mapped(self, f)

  def mapWithKeys[W](f: (K, V) => W): Map[K, W] = new MapT.MappedWithKeys(self, f)

  /**
   * Returns the Cartesian product map of two maps. $LAZY
   * @example {{{
   *   {1 -> 'A', 2 -> 'B'} product {true -> 1, false -> 0} ==
   *     { (1, true)  -> ('A', 1),
   *       (1, false) -> ('A', 0),
   *       (2, true)  -> ('B', 1),
   *       (2, false) -> ('B', 0) }
   * }}}
   */
  def product[L, W](that: Map[L, W]): Map[(K, L), (V, W)] = new MapT.Product(self, that)

  /**
   * Zips two maps with the same key type into one map that maps keys to a pair of values. $LAZY
   *
   * @note This function is not the same as the Scala library's `zip`. Please
   *       use `pairs.zip` instead for zipping a sequence of pairs.
   * @example {{{{1 -> 2, 2 -> 3} zip {2 -> 5, 3 -> 6} == {2 -> (3, 5)} }}}
   */
  def zip[W](that: Map[K, W]): Map[K, (V, W)] = (self zipWith that) { (v, w) => (v, w) }

  /**
   * @note `(a zipWith b)(f)` is equivalent to `a zip b map f` but may be faster.
   */
  def zipWith[W, X](that: Map[K, W])(f: (V, W) => X): Map[K, X] = new MapT.ZippedWith(self, that, f)

  /**
   * Returns the inner join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self INNER JOIN that ON self.key == that.key`.
   */
  def innerJoin[W](that: Map[K, W]) = self zip that

  /**
   * Returns the left outer join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self LEFT OUTER JOIN that ON self.key == that.key`.
   */
  def leftOuterJoin[W](that: Map[K, W]): Map[K, (V, Option[W])] = new MapT.LeftOuterJoined(self, that)

  /**
   * Returns the right outer join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self RIGHT OUTER JOIN that ON self.key == that.key`.
   */
  def rightOuterJoin[W](that: Map[K, W]): Map[K, (Option[V], W)] = new MapT.RightOuterJoined(self, that)

  /**
   * Returns the full outer join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self FULL OUTER JOIN that ON self.key == that.key`.
   */
  def fullOuterJoin[W](that: Map[K, W]): Map[K, (Option[V], Option[W])] = new MapT.FullOuterJoined(self, that)

  /**
   * Returns the symmetric difference of the two maps. $LAZY
   * @example {{{
   *   {1 -> a, 2 -> b} symmetricDiff
   *   {2 -> b, 3 -> c} == {1 -> Left(a), 3 -> Right(c)}
   * }}}
   */
  def symmetricDiff[W](that: Map[K, W]): Map[K, Either[V, W]] = new MapT.SymmetricDiff(self, that)

  /**
   * Wraps the keys of this map with a bijection. $LAZY
   * {{{
   *   K => V              J <=> K          J => V
   *    self  . contramap  ( that )    ==   result
   * }}}
 *
   * @example {{{
   *   {1 -> 'A', 2 -> 'B'} contramap {'a' <-> 1, 'b' <-> 2}
   *   == {'a' -> 'A', 'b' -> 'B'}
   * }}}
   */
  def contramap[J](f: Bijection[J, K]): Map[J, V] = new MapT.Contramapped(self, f)

  /**
   * Wraps around this map and modified its behavior:
   * When an absent key is accessed, returns the given default value. But this key would not be added to the map.
   */
  def withDefault[W >: V](default: => W): Map[K, W] = new MapT.WithDefault[K, V, W](self, default)

  def asMap: Map[K, V] = new MapT.Bare(self)

  def asMultimap[W >: V](implicit W: Eq[W]): Multimap[K, W] = new MapT.AsMultimap(self, W)

  def asMultiset[W >: V](implicit W: OrderedRing[W]): Multiset[K, W] = new MapT.AsMultiset(self, W)

  // SYMBOLIC ALIASES
  def ×[L, W](that: Map[L, W]) = self product that

  override def |>[W](f: V => W) = self map f
  def |>:[J](f: Bijection[J, K]) = self contramap f

  def ⋈[W](that: Map[K, W]) = self innerJoin that
  def ⟕[W](that: Map[K, W]) = self leftOuterJoin that
  def ⟖[W](that: Map[K, W]) = self rightOuterJoin that
  def ⟗[W](that: Map[K, W]) = self fullOuterJoin that

  // OVERRIDING JAVA METHODS
  override def toString = "{" + pairs.map { case (k, v) => s"$k → $v" }.buildString(", ") + "}"

  override def equals(that: Any) = that match {
    case that: Map[K, V] => Map.Eq(Eq.default[V]).eq(this, that)
    case _ => false
  }

  override def hashCode = MurmurHash3.symmetricHash(self.pairs)(Hashing.default[(K, V)])

}

object Map extends FactoryAB_EvA[Map, Eq] with MapLowPriorityTypeclassInstances {

  // CONSTRUCTORS

  def from[K: Eq, V](kvs: Traversable[(K, V)]) = AutoMap from kvs

  def empty[K: Eq]: Map[K, Nothing] = new AbstractMap[K, Nothing] {
    def apply(k: K) = throw new KeyNotFoundException[K](k)
    def ?(k: K) = None
    def keySet = Set.empty[K]
  }

  // IMPLICIT CONVERSIONS
  implicit class MapWhoseKeysArePairsOps[K, L, V](val m: Map[(K, L), V]) extends AnyVal {
    /**
     * Returns the curried version of a map whose key is a pair.
     * {{{
     *   Map[(K, L), V] . curry == Map[K, Map[L, V]]
     *                  ANALOGOUS TO
     *    (K, L) => V   . curry ==  K => (L => V)
     * }}}
     * @note The user should guarantee that the implicit equivalence relation of K and L
     *       conforms to the equivalence relation on pair (K, L) stored in this map. I.e.,
     *       `(K product L)` should behave exactly the same as [[Map.keyEq]].
     * @note This function incurs some overhead (traversing through the key set).
     */
    def curry(implicit K: Eq[K], L: Eq[L]): Map[K, Map[L, V]] = new AbstractMap[K, Map[L, V]] {
      private[this] val domK = AutoSet[K]()
      private[this] val domL = AutoSet[L]()
      for ((k, l) <- m.keys) {
        domK += k
        domL += l
      }
      def keySet = domK
      def ?(k: K) = if (domK contains k) Some(apply(k)) else None
      def apply(k: K) = domL createMapOptionally { l => m ? (k, l) }
    }
  }

  implicit class MapWhoseValuesAreMapsOps[K, L, V](val m: Map[K, Map[L, V]]) extends AnyVal {
    /**
     * Returns the uncurried version of a map whose value is also a map.
     * {{{
     *   Map[K, Map[L, V]] . uncurry == Map[(K, L), V]
     *                     ANALOGOUS TO
     *     K => (L => V)   . uncurry ==  (K, L) => V
     * }}}
     * @note The user should guarantee that the implicit equivalence relation on L
     *       conforms with every inner map (of type `Map[L, V]`) of the curried map.
     */
    def uncurry(implicit L: Eq[L]): Map[(K, L), V] = new AbstractMap[(K, L), V] {
      def keySet = new AbstractSet[(K, L)] {
        def keyEq = m.keyEq product L
        def keys = for ((k, ml) <- m.pairs; l <- ml.keys) yield (k, l)
        def contains(kl: (K, L)) =  m.containsKey(kl._1) && m(kl._1).containsKey(kl._2)
      }
      def ?(kl: (K, L)) = for (ml <- m ? kl._1) yield ml(kl._2)
      override def pairs = for ((k, ml) <- m.pairs; (l, v) <- ml.pairs) yield ((k, l), v)
      def apply(kl: (K, L)) = m(kl._1)(kl._2)
    }
  }

  // TYPECLASS INSTANCES

  implicit def __dynamicEq[K, V](implicit V: Eq[V]): Eq[Map[K, V]] = V match {
    case vh: Hashing[V] => ???

    case ve => new Eq[Map[K, V]] {
      def eq(x: Map[K, V], y: Map[K, V]) = (x, y) match {
        case (x: IndexedSeq[V], y: IndexedSeq[V]) => IndexedSeq.Eq[V].eq(x, y)
        case (x: Seq[V], y: Seq[V]) => Seq.Eq[V].eq(x, y)
        case (x: Table[V], y: Table[V]) => Table.Eq[V].eq(x, y)
        case _ => Map.Eq[K, V].eq(x, y)
      }
    }
  }

  //TODO: should be implicit, but contravariant typeclass implicit resolution is buggy (SI-2509)
  /** Returns the equivalence relation on maps as long as there is an equivalence relation on the value type. */
  def Eq[K, V: Eq]: Eq[Map[K, V]] = new MapT.MapEq[K, V]

  /** Returns the functor on maps. */
  //TODO: this should be stronger than a functor but less than an applicative functor (no unit element)
  implicit def Functor[K]: Functor[({type λ[+V] = Map[K, V]})#λ] = new Functor[({type λ[+V] = Map[K, V]})#λ] {
    def map[X, Y](mx: Map[K, X])(f: X => Y): Map[K, Y] = mx map f
  }

  /** Returns the vector space on maps given the value set of the map forms a field. */
  implicit def VectorSpace[K: Eq, F: Field]: VectorSpace[Map[K, F], F] = new VectorSpace[Map[K, F], F] {
    def scalarField = Field[F]
    def scale(x: Map[K, F], k: F) = x map (_ * k)
    def add(x: Map[K, F], y: Map[K, F]) = (x fullOuterJoin y) map {
      case (Some(a), Some(b)) => a + b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case _ => function.zero[F]
    }
    def zero = Map.empty[K]
  }

}

trait MapLowPriorityTypeclassInstances {
  /** Returns the module on maps given the value set of the map forms a ring. */
  implicit def Module[K: Eq, R: Ring]: Module[Map[K, R], R] = new Module[Map[K, R], R] {
    private[this] val R = Ring[R]
    def scalarRing = R
    def scale(x: Map[K, R], k: R) = x map (_ * k)
    def add(x: Map[K, R], y: Map[K, R]) = (x fullOuterJoin y) map {
      case (Some(a), Some(b)) => a + b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case _ => R.zero
    }
    def zero = Map.empty[K]
  }
}


abstract class AbstractMap[@sp(Int) K, +V] extends Map[K, V]

private[poly] object MapT {

  class MapEq[K, V: Eq] extends Eq[Map[K, V]] {
    def eq(x: Map[K, V], y: Map[K, V]) =
      (x.keys forall { k => x(k) === y(k) }) && (y.keys forall { k => y(k) === x(k) })
  }

  class KeyFiltered[K, V](self: Map[K, V], f: K => Boolean) extends AbstractMap[K, V] {
    def apply(k: K) = if (!f(k)) throw new KeyNotFoundException(k) else self(k)
    def ?(k: K) = if (!f(k)) None else self ? k
    def keySet = self.keySet filter f
    override def pairs = self.pairs.filter { case (k, _) => f(k) }
  }

  class Mapped[K, V, W](self: Map[K, V], f: V => W) extends AbstractMap[K, W] {
    def keySet = self.keySet
    def ?(x: K) = (self ? x).map(f)
    def apply(x: K) = f(self(x))
    override def pairs = self.pairs.map { case (k, v) => (k, f(v)) }
    override def size = self.size
  }

  class MappedWithKeys[K, V, W](self: Map[K, V], f: (K, V) => W) extends AbstractMap[K, W] {
    def keySet = self.keySet
    def ?(k: K) = for (v <- self ? k) yield f(k, v)
    def apply(k: K) = f(k, self(k))
    override def pairs = self.pairs.map { case (k, v) => (k, f(k, v)) }
  }

  class Contramapped[K, V, J](self: Map[K, V], f: Bijection[J, K]) extends AbstractMap[J, V] {
    def keySet = self.keySet contramap f
    override def pairs = self.pairs.map { case (k, v) => (f.invert(k), v) }
    def apply(k: J) = self apply f(k)
    def ?(k: J) = self ? f(k)
  }

  class Product[K, L, V, W](self: Map[K, V], that: Map[L, W]) extends AbstractMap[(K, L), (V, W)] {
    def keySet = self.keySet product that.keySet
    def ?(k: (K, L)) = for (v <- self ? k._1; v1 <- that ? k._2) yield (v, v1)
    def apply(k: (K, L)) = (self(k._1), that(k._2))
    override def pairs = for ((k, v) <- self.pairs; (l, w) <- that.pairs) yield ((k, l) -> (v, w))
    override def size = self.size * that.size
  }

  class ZippedWith[K, V, W, X](self: Map[K, V], that: Map[K, W], f: (V, W) => X) extends AbstractMap[K, X] {
    def keySet = self.keySet intersect that.keySet
    def ?(k: K) = for (v <- self ? k; w <- that ? k) yield f(v, w)
    override def pairs = self.pairs filter { case (k, v) => that containsKey k } map { case (k, v) => (k, f(v, that(k))) }
    def apply(k: K) = f(self(k), that(k))
  }

  class LeftOuterJoined[K, V, W](self: Map[K, V], that: Map[K, W]) extends AbstractMap[K, (V, Option[W])] {
    def keySet = self.keySet
    def apply(k: K) = (self(k), that ? k)
    def ?(k: K) = for (v <- self ? k) yield (v, that ? k)
    override def pairs = self.pairs map { case (k, v) => (k, (v, that ? k)) }
  }

  class RightOuterJoined[K, V, W](self: Map[K, V], that: Map[K, W]) extends AbstractMap[K, (Option[V], W)] {
    def keySet = that.keySet
    def apply(k: K) = (self ? k, that(k))
    def ?(k: K) = for (w <- that ? k) yield (self ? k, w)
    override def pairs = that.pairs map { case (k, w) => (k, (self ? k, w)) }
  }

  class FullOuterJoined[K, V, W](self: Map[K, V], that: Map[K, W]) extends AbstractMap[K, (Option[V], Option[W])] {
    def keySet = self.keySet union that.keySet
    def apply(k: K) = (self ? k, that ? k)
    def ?(k: K) = (self ? k, that ? k) match {
      case (None, None) => None
      case res => Some(res)
    }
  }

  class SymmetricDiff[K, V, W](self: Map[K, V], that: Map[K, W]) extends AbstractMap[K, Either[V, W]] {
    def keySet = self.keySet symmetricDiff that.keySet
    def ?(k: K) = (self containsKey k, that containsKey k) match {
      case (true, false) => Some(Left(self(k)))
      case (false, true) => Some(Right(that(k)))
      case _ => None
    }
    override def pairs = {
      val l = for ((k, v) <- self.pairs if that notContainsKey k) yield (k, Left(v))
      val r = for ((k, v) <- that.pairs if self notContainsKey k) yield (k, Right(v))
      l ++ r
    }
    def apply(k: K) = Either.cond(that.containsKey(k), that(k), self(k))
  }

  class WithDefault[K, V, W >: V](self: Map[K, V], default: => W) extends AbstractMap[K, W] {
    def keySet = self.keySet
    override def pairs = self.pairs
    def apply(k: K) = (self ? k).getOrElse(default)
    def ?(k: K) = self ? k
  }

  class Bare[K, V](self: Map[K, V]) extends AbstractMap[K, V] {
    def keySet = self.keySet
    def apply(k: K) = self.apply(k)
    def ?(k: K) = self ? k
    override def pairs = self.pairs
  }

  class AsMultimap[K, V](self: Map[K, V], implicit val valueEq: Eq[V]) extends AbstractMultimap[K, V] {
    def keySet = self.keySet
    override def pairs = self.pairs
    def apply(k: K) = self ? k match {
      case Some(v) => Set(v)
      case None => Set.empty[V]
    }
  }

  class AsMultiset[K, R](self: Map[K, R], implicit val R: OrderedRing[R]) extends AbstractMultiset[K, R] {
    implicit def weightRing = R
    def keySet = self.keySet
    def weight(k: K) = if (self.containsKey(k)) R.one else R.zero
  }
}
