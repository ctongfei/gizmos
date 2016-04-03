package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.hkt._
import poly.algebra.specgroup._
import poly.collection.exception._
import scala.language.reflectiveCalls

/**
 * The base trait for maps.
 * A map is a mapping between a key type (domain) and a value type (codomain).
 * It can also be viewed as a collection of (key, value) pairs, in which each key is unique.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Map[@sp(i) K, +V] extends KeyedLike[K, Map[K, V]] with PartialFunction[K, V] { self =>

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
   * For maximum safety, use `?` to optionally access an element.
   * @param k The given key
   * @return The associated value of ''k''
   * @throws KeyNotFoundException if key not found (may or may not throw)
   */
  def apply(k: K): V

  /** Returns the number of (key, value) pairs this map contains. */
  def size: Int = pairs.size

  /**
   * Checks if the specified key is present in this map.
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
    def equivOnKey = self.equivOnKey
    def contains(x: K): Boolean = self.containsKey(x)
    override def size: Int = self.size
    def keys: Iterable[K] = self.pairs.map(firstOfPair)
  }

  /** Returns an iterable collection of the keys in this map. $LAZY */
  def keys = pairs map firstOfPair

  /** Returns an iterable collection of the values in this map. $LAZY */
  def values = pairs map secondOfPair

  // HELPER FUNCTIONS

  def filterKeys(f: K => Boolean): Map[K, V] = new AbstractMap[K, V] {
    def apply(k: K) = if (!f(k)) throw new KeyNotFoundException(k) else self(k)
    def ?(k: K) = if (!f(k)) None else self ? k
    def pairs = self.pairs.filter { case (k, _) => f(k) }
    def containsKey(k: K) = if (!f(k)) false else self.containsKey(k)
    def equivOnKey = self.equivOnKey
  }

  /**
   * Transforms the values of this map according to the specified function.
   * This is the functor operation on maps. $LAZY
   * {{{
   *   K => V        V => W          K => W
   *    self  . map ( that )    ==   result
   * }}}
   * @note This function is equivalent to the Scala library's `mapValues`.
   *       To transform all pairs in this map, use `this.pairs.map`.
   * @example {{{Map(1 -> 2, 2 -> 3) map {_ * 2} == Map(1 -> 4, 2 -> 6)}}}
   * @param f The specific function
   * @return A map view that maps every key of this map to `f(this(key))`.
   */
  def map[W](f: V => W): Map[K, W] = new AbstractMap[K, W] {
    def equivOnKey = self.equivOnKey
    def containsKey(x: K) = self.containsKey(x)
    def ?(x: K) = (self ? x).map(f)
    def apply(x: K) = f(self(x))
    def pairs = self.pairs.map { case (k, v) => (k, f(v)) }
    override def size = self.size
  }

  /**
   * Returns the product map of two maps. $LAZY
   * @example {{{
   *   Map(1 -> 'A', 2 -> 'B') product Map(true -> 1, false -> 0) ==
   *   Map((1, true)  -> ('A', 1),
   *       (1, false) -> ('A', 0),
   *       (2, true)  -> ('B', 1),
   *       (2, false) -> ('B', 0))
   * }}}
   */
  def cartesianProduct[L, W](that: Map[L, W]): Map[(K, L), (V, W)] = new AbstractMap[(K, L), (V, W)] {
    def equivOnKey = Equiv.product(self.equivOnKey, that.equivOnKey)
    def containsKey(k: (K, L)) = self.containsKey(k._1) && that.containsKey(k._2)
    def ?(k: (K, L)) = for (v ← self ? k._1; v1 ← that ? k._2) yield (v, v1)
    def apply(k: (K, L)) = (self(k._1), that(k._2))
    def pairs = for (k ← self.keys; k1 ← that.keys) yield ((k, k1), (self(k), that(k1)))
    override def size = self.size * that.size
  }

  /**
   * Zips two maps with the same key type into one. $LAZY
   * @note This function is not the same as the Scala library's `zip`. Please
   *       use `this.pairs.zip` instead for zipping a sequence of pairs.
   * @example {{{Map(1 -> 2, 2 -> 3) zip Map(2 -> 5, 3 -> 6) == Map(2 -> (3, 5))}}}
   * @param that Another map to be zipped
   */
  def zip[W](that: Map[K, W]): Map[K, (V, W)] = new AbstractMap[K, (V, W)] {
    def equivOnKey = self.equivOnKey
    def apply(x: K) = (self(x), that(x))
    def ?(x: K) = for (v ← self ? x; w ← that ? x) yield (v, w)
    def pairs = self.pairs filter { case (k, v) => that containsKey k } map { case (k, v) => (k, (v, that(k))) }
    def containsKey(x: K) = self.containsKey(x) && that.containsKey(x)
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
    def equivOnKey = self.equivOnKey
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
    def equivOnKey = that.equivOnKey
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
    def equivOnKey = self.equivOnKey
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
   * @example {{{
   *   Map(1 -> 'A', 2 -> 'B') contramap
   *   BijectiveMap('a' <-> 1, 'b' <-> 2) == Map('a' -> 'A', 'b' -> 'B')
   * }}}
   */
  def contramap[J](f: Bijection[J, K]): Map[J, V] = new AbstractMap[J, V] {
    def pairs = self.pairs.map { case (k, v) => (f.invert(k), v) }
    def containsKey(x: J) = self containsKey f(x)
    def apply(k: J) = self apply f(k)
    def ?(k: J) = self ? f(k)
    implicit def equivOnKey = self.equivOnKey contramap f
  }

  def withDefault[W >: V](default: W): Map[K, W] = new AbstractMap[K, W] {
    def pairs = self.pairs
    def containsKey(x: K) = self.containsKey(x)
    def apply(k: K) = (self ? k).getOrElse(default)
    def ?(k: K) = self ? k
    def equivOnKey = self.equivOnKey
}

  def asMap: Map[K, V] = new AbstractMap[K, V] {
    def apply(k: K) = self.apply(k)
    def ?(k: K) = self ? k
    implicit def equivOnKey = self.equivOnKey
    def pairs = self.pairs
    def containsKey(x: K) = self.containsKey(x)
  }

  override def toString = "Map{" + pairs.map { case (k, v) => s"$k → $v" }.buildString(", ") + "}"

  def |>[W](f: V => W) = self map f
  def |<[J](f: Bijection[J, K]) = self contramap f
  def |~|[W](that: Map[K, W]) = self zip that

  def ⋈[W](that: Map[K, W]) = self innerJoin that
  def ⟕[W](that: Map[K, W]) = self leftOuterJoin that
  def ⟖[W](that: Map[K, W]) = self rightOuterJoin that
  def ⟗[W](that: Map[K, W]) = self fullOuterJoin that

}

object Map extends MapLowPriorityImplicits {

  def empty[K: Equiv]: Map[K, Nothing] = new AbstractMap[K, Nothing] {
    def apply(k: K) = throw new KeyNotFoundException[K](k)
    def ?(k: K) = None
    def equivOnKey = Equiv[K]
    def pairs = Iterable.empty
    def containsKey(x: K) = false
  }

  // TYPECLASS INSTANCES

  /** Returns the functor on maps. */
  implicit def Functor[K]: Functor[({type λ[+V] = Map[K, V]})#λ] = new Functor[({type λ[+V] = Map[K, V]})#λ] {
    def map[X, Y](mx: Map[K, X])(f: X => Y): Map[K, Y] = mx map f
  }

  /** Returns the vector space on maps given the value set of the map forms a field. */
  implicit def VectorSpace[K: Equiv, F: Field]: VectorSpace[Map[K, F], F] = new VectorSpace[Map[K, F], F] {
    private[this] val F = Field[F]
    def fieldOnScalar = F
    def scale(x: Map[K, F], k: F) = x map (_ * k)
    def add(x: Map[K, F], y: Map[K, F]) = (x fullOuterJoin y) map {
      case (Some(a), Some(b)) => a + b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case _ => F.zero
    }
    def zero = empty[K]
  }
}

trait MapLowPriorityImplicits {
  /** Returns the module on maps given the value set of the map forms a ring. */
  implicit def Module[K: Equiv, R: Ring]: Module[Map[K, R], R] = new Module[Map[K, R], R] {
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

abstract class AbstractMap[@sp(i) K, +V] extends Map[K, V]

