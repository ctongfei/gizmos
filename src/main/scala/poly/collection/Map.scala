package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.collection.exception._
import poly.util.specgroup._
import scala.language.reflectiveCalls

/**
 * The base trait for maps.
 * A map is a mapping between a key type (domain) and a value type (codomain).
 * It can also be viewed as a collection of (key, value) pairs, in which each key is unique.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Map[@sp(i) K, +V] extends Keyed[K] with PartialFunction[K, V] { self =>

  /**
   * Returns all key-value pairs stored in this map.
   * @return An iterable sequence of key-value pairs.
   */
  def pairs: Iterable[(K, V)]

  /**
   * Optionally retrieves the value associated with the specified key.
   * @param k The given key
   * @return The associated value. If the key is not found, return [[None]].
   */
  def ?(k: K): Option[V]

  /**
   * Retrieves the value associated with the specified key.
   * If the key is not found, its behavior is undefined (this is a deliberate design for efficiency).
   * For maximum safety, use `?` to optionally access an element.
   * @param k The given key
   * @return The associated value
   * @throws NoSuchElementException if key not found (may or may not throw)
   */
  def apply(k: K): V

  /** Returns the number of (key, value) pairs this map contains. */
  def size: Int

  /**
   * Checks if the specified key is present in this map.
   * @param x The given key
   * @return Whether the key exists in this map
   */
  def containsKey(x: K): Boolean

  def getOrElse[W >: V](x: K, default: => W) = ?(x) match {
    case Some(y) => y
    case None => default
  }

  def isDefinedAt(x: K) = containsKey(x)

  def keySet: Set[K] = new Set[K] {
    def equivOnKey = self.equivOnKey
    def contains(x: K): Boolean = self.containsKey(x)
    def size: Int = self.size
    def elements: Iterable[K] = self.pairs.map(_._1)
  }

  def keys = self.pairs.map(_._1)

  def values = self.pairs.map(_._2)

  // HELPER FUNCTIONS
  /**
   * Transforms the values of this map according to the specified function.
   * @note This function is equivalent to the Scala library's `mapValues`.
   *       To transform all pairs in this map, use `this.pairs.map`.
   * @example {{{Map(1 -> 2, 2 -> 3) map {_ * 2} == Map(1 -> 4, 2 -> 6)}}}
   * @param f The specific function
   * @return A map view that maps every key of this map to `f(this(key))`.
   */
  def map[W](f: V => W): Map[K, W] = new AbstractMap[K, W] {
    def equivOnKey = self.equivOnKey
    def containsKey(x: K): Boolean = self.containsKey(x)
    def ?(x: K): Option[W] = (self ? x).map(f)
    def apply(x: K): W = f(self(x))
    def pairs = self.pairs.map { case (k, v) => (k, f(v)) }
    def size: Int = self.size
  }


  /**
   * Zips two maps with the same key type into one.
   * @note This function is not the same as the Scala library's `zip`. Please
   *       use `this.pairs.zip` instead for zipping pairs.
   * @example {{{Map(1 -> 2, 2 -> 3) zip Map(2 -> 5, 3 -> 6) == Map(2 -> (3, 5))}}}
   * @param that Another map to be zipped
   */
  def zip[W](that: Map[K, W]): Map[K, (V, W)] = {
    require(this.equivOnKey equivSameAs that.equivOnKey)
    new AbstractMap[K, (V, W)] {
      def equivOnKey = self.equivOnKey
      def apply(x: K): (V, W) = (self(x), that(x))
      def ?(x: K): Option[(V, W)] = for {v ← self ? x; w ← that ? x} yield (v, w)
      def pairs = self.pairs filter { case (k, v) => that containsKey k } map { case (k, v) => (k, (v, that(k))) }
      def size: Int = pairs.size
      def containsKey(x: K): Boolean = self.containsKey(x) && that.containsKey(x)
    }
  }

  def |>[W](f: V => W) = self map f
}

object Map {
  /** Returns the functor on maps. */
  implicit def Functor[K]: Functor[({type λ[+V] = Map[K, V]})#λ] = new Functor[({type λ[+V] = Map[K, V]})#λ] {
    def map[X, Y](mx: Map[K, X])(f: X => Y): Map[K, Y] = mx map f
  }

}

abstract class AbstractMap[@sp(i) K, +V] extends Map[K, V]
