package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.hkt._
import poly.algebra.specgroup._
import poly.collection.conversion.FromScala._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.mut._

import scala.language.reflectiveCalls

/**
 * The base trait for maps.
 * A map is a mapping between a key type (domain) and a value type (codomain).
 * It can also be viewed as a collection of (key, value) pairs, in which each key is unique.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Map[@sp(Int) K, +V] extends KeyedLike[K, Map[K, V]] with PartialFunction[K, V] { self =>

  /**
   * Returns all key-value pairs stored in this map.
   * @return An iterable sequence of key-value pairs.
   */
  def pairs: Iterable[(K, V)]

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

  /** Returns the number of (key, value) pairs this map contains. */
  def size: Int = pairs.size

  /**
   * Checks if the specified key is present in this map.
   *
   * @return Whether the key exists in this map
   */
  def containsKey(x: K): Boolean

  /**
   * Returns the value associated with the given key,
   * or a default value if the key is not present in the map.
   */
  def getOrElse[W >: V](x: K, default: => W) = ?(x) match {
    case Some(y) => y
    case None => default
  }

  def isDefinedAt(x: K) = containsKey(x)

  /** Returns the set of the keys of this map. $LAZY */
  def keySet: Set[K] = new AbstractSet[K] {
    def eqOnKeys = self.eqOnKeys
    def contains(x: K) = self.containsKey(x)
    override def size = self.size
    def keys = self.pairs map firstOfPair
  }

  /** Returns an iterable collection of the keys in this map. $LAZY */
  def keys = pairs map firstOfPair

  /** Returns an iterable collection of the values in this map. $LAZY */
  def values = pairs map secondOfPair

  // HELPER FUNCTIONS

  def isEmpty = size != 0

  def filterKeys(f: K => Boolean): Map[K, V] = new AbstractMap[K, V] {
    def apply(k: K) = if (!f(k)) throw new KeyNotFoundException(k) else self(k)
    def ?(k: K) = if (!f(k)) None else self ? k
    def pairs = self.pairs.filter { case (k, _) => f(k) }
    def containsKey(k: K) = if (!f(k)) false else self.containsKey(k)

    def eqOnKeys = self.eqOnKeys
  }

  /**
   * Transforms the values of this map according to the specified function.
   * This is the functor operation on maps. $LAZY
   * {{{
   *   K => V        V => W          K => W
   *    self  . map ( that )    ==   result
   * }}}
   *
   * @note This function is equivalent to the Scala library's `mapValues`.
   *       To transform all pairs in this map, use `this.pairs.map`.
   * @example {{{ {1 -> 2, 2 -> 3} map {_ * 2} == {1 -> 4, 2 -> 6} }}}
   * @param f The specific function
   * @return A map view that maps every key of this map to `f(this(key))`.
   */
  def map[W](f: V => W): Map[K, W] = new AbstractMap[K, W] {
    def eqOnKeys = self.eqOnKeys
    def containsKey(x: K) = self.containsKey(x)
    def ?(x: K) = (self ? x).map(f)
    def apply(x: K) = f(self(x))
    def pairs = self.pairs.map { case (k, v) => (k, f(v)) }
    override def size = self.size
  }

  /**
   * Returns the product map of two maps. $LAZY
   *
   * @example {{{
   *   {1 -> 'A', 2 -> 'B'} product {true -> 1, false -> 0} ==
   *      {(1, true)  -> ('A', 1),
   *       (1, false) -> ('A', 0),
   *       (2, true)  -> ('B', 1),
   *       (2, false) -> ('B', 0)}
   * }}}
   */
  def cartesianProduct[L, W](that: Map[L, W]): Map[(K, L), (V, W)] = new AbstractMap[(K, L), (V, W)] {
    def eqOnKeys = Eq.product(self.eqOnKeys, that.eqOnKeys)
    def containsKey(k: (K, L)) = self.containsKey(k._1) && that.containsKey(k._2)
    def ?(k: (K, L)) = for (v ← self ? k._1; v1 ← that ? k._2) yield (v, v1)
    def apply(k: (K, L)) = (self(k._1), that(k._2))
    def pairs = for (k ← self.keys; k1 ← that.keys) yield ((k, k1), (self(k), that(k1)))
    override def size = self.size * that.size
  }

  /**
   * Zips two maps with the same key type into one map that maps keys to a pair of values. $LAZY
   *
   * @note This function is not the same as the Scala library's `zip`. Please
   *       use `pairs.zip` instead for zipping a sequence of pairs.
   * @example {{{{1 -> 2, 2 -> 3} zip {2 -> 5, 3 -> 6} == {2 -> (3, 5)} }}}
   */
  def zip[W](that: Map[K, W]): Map[K, (V, W)] = new AbstractMap[K, (V, W)] {
    def eqOnKeys = self.eqOnKeys
    def apply(x: K) = (self(x), that(x))
    def ?(x: K) = for (v ← self ? x; w ← that ? x) yield (v, w)
    def pairs = self.pairs filter { case (k, v) => that containsKey k } map { case (k, v) => (k, (v, that(k))) }
    def containsKey(x: K) = self.containsKey(x) && that.containsKey(x)
  }

  def zipWith[W, X](that: Map[K, W])(f: (V, W) => X): Map[K, X] = new AbstractMap[K, X] {
    def ?(k: K) = for (v ← self ? k; w ← that ? k) yield f(v, w)
    def pairs = self.pairs filter { case (k, v) => that containsKey k } map { case (k, v) => (k, f(v, that(k))) }
    def containsKey(x: K) = self.containsKey(x) && that.containsKey(x)
    def apply(k: K) = f(self(k), that(k))
    def eqOnKeys = self.eqOnKeys
}

  /**
   * Returns the inner join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self INNER JOIN that ON self.key == that.key`.
   */
  def innerJoin[W](that: Map[K, W]) = self zip that

  /**
   * Returns the left outer join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self LEFT OUTER JOIN that ON self.key == that.key`.
   */
  def leftOuterJoin[W](that: Map[K, W]): Map[K, (V, Option[W])] = new AbstractMap[K, (V, Option[W])] {
    def apply(k: K) = (self(k), that ? k)
    def ?(k: K) = for (v ← self ? k) yield (v, that ? k)

    def eqOnKeys = self.eqOnKeys
    def pairs = self.pairs map { case (k, v) => (k, (v, that ? k)) }
    def containsKey(x: K) = self containsKey x
  }

  /**
   * Returns the right outer join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self RIGHT OUTER JOIN that ON self.key == that.key`.
   */
  def rightOuterJoin[W](that: Map[K, W]): Map[K, (Option[V], W)] = new AbstractMap[K, (Option[V], W)] {
    def apply(k: K) = (self ? k, that(k))
    def ?(k: K) = for (w ← that ? k) yield (self ? k, w)

    def eqOnKeys = that.eqOnKeys
    def pairs = that.pairs map { case (k, w) => (k, (self ? k, w)) }
    def containsKey(x: K) = that containsKey x
  }

  /**
   * Returns the full outer join of two maps by their keys.
   * It is similar to the SQL expression `SELECT * FROM self FULL OUTER JOIN that ON self.key == that.key`.
   */
  def fullOuterJoin[W](that: Map[K, W]): Map[K, (Option[V], Option[W])] = new AbstractMap[K, (Option[V], Option[W])] {
    def apply(k: K) = (self ? k, that ? k)
    def ?(k: K) = (self ? k, that ? k) match {
      case (None, None) => None
      case res => Some(res)
    }
    def eqOnKeys = self.eqOnKeys
    def pairs =
      (self.pairs map { case (k, v) => k → (Some(v), that ? k) }) ++
      (that.pairs filter { case (k, w) => self notContainsKey k } map { case (k, w) => k → (None, Some(w)) })
    def containsKey(x: K) = self.containsKey(x) || that.containsKey(x)
  }

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
  def contramap[J](f: Bijection[J, K]): Map[J, V] = new AbstractMap[J, V] {
    def pairs = self.pairs.map { case (k, v) => (f.invert(k), v) }
    def containsKey(x: J) = self containsKey f(x)
    def apply(k: J) = self apply f(k)
    def ?(k: J) = self ? f(k)

    implicit def eqOnKeys = self.eqOnKeys contramap f
  }

  def withDefault[W >: V](default: => W): Map[K, W] = new AbstractMap[K, W] {
    def pairs = self.pairs
    def containsKey(x: K) = self.containsKey(x)
    def apply(k: K) = (self ? k).getOrElse(default)
    def ?(k: K) = self ? k

    def eqOnKeys = self.eqOnKeys
}

  def asMap: Map[K, V] = new AbstractMap[K, V] {
    def apply(k: K) = self.apply(k)
    def ?(k: K) = self ? k

    implicit def eqOnKeys = self.eqOnKeys
    def pairs = self.pairs
    def containsKey(x: K) = self.containsKey(x)
  }

  // SYMBOLIC ALIASES
  def |~|[W](that: Map[K, W]) = self zip that

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

object Map extends Factory2Ev[Map, Eq] with MapLowPriorityTypeclassInstances {

  // CONSTRUCTORS

  def from[K: Eq, V](kvs: Traversable[(K, V)]) = AutoMap from kvs

  def empty[K: Eq]: Map[K, Nothing] = new AbstractMap[K, Nothing] {
    def apply(k: K) = throw new KeyNotFoundException[K](k)
    def ?(k: K) = None
    def eqOnKeys = poly.algebra.Eq[K]
    def pairs = Iterable.empty
    def containsKey(x: K) = false
  }

  // TYPECLASS INSTANCES

  //TODO: dynamic resolution of implicit Eq based on runtime type
  //TODO: this should be resolved at compile time
  implicit def __dynamicEq[K, V: Eq]: Eq[Map[K, V]] = new Eq[Map[K, V]] {
    def eq(x: Map[K, V], y: Map[K, V]) = (x, y) match {
      case (x: IndexedSeq[V], y: IndexedSeq[V]) => IndexedSeq.Eq[V].eq(x, y)
      case (x: Seq[V], y: Seq[V]) => Seq.Eq[V].eq(x, y)
      case _ => Map.Eq[K, V].eq(x, y)
    }
  }

  //TODO: should be implicit, but results in ambiguous implicits because of contravariant typeclass
  /** Returns the equivalence relation on maps as long as there is an equivalence relation on the value type. */
  def Eq[K, V: Eq]: Eq[Map[K, V]] = new Eq[Map[K, V]] {
    def eq(x: Map[K, V], y: Map[K, V]) =
      (x.keys forall { k => x(k) === y(k) }) && (y.keys forall { k => y(k) === x(k) })
  }

  /** Returns the functor on maps. */
  implicit def Functor[K]: Functor[({type λ[+V] = Map[K, V]})#λ] = new Functor[({type λ[+V] = Map[K, V]})#λ] {
    def map[X, Y](mx: Map[K, X])(f: X => Y): Map[K, Y] = mx map f
  }

  /** Returns the vector space on maps given the value set of the map forms a field. */
  implicit def VectorSpace[K: Eq, F: Field]: VectorSpace[Map[K, F], F] = new VectorSpace[Map[K, F], F] {
    private[this] val F = Field[F]
    def fieldOnScalar = F
    def scale(x: Map[K, F], k: F) = x map (_ * k)
    def add(x: Map[K, F], y: Map[K, F]) = (x fullOuterJoin y) map {
      case (Some(a), Some(b)) => a + b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case _ => F.zero
    }
    def zero = Map.empty[K]
  }

}

trait MapLowPriorityTypeclassInstances {
  /** Returns the module on maps given the value set of the map forms a ring. */
  implicit def Module[K: Eq, R: Ring]: Module[Map[K, R], R] = new Module[Map[K, R], R] {
    private[this] val R = Ring[R]
    def ringOnScalar = R
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

