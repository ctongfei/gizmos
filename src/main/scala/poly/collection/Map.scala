package poly.collection

import poly.algebra.hkt._
import poly.collection.exception._
import poly.util.specgroup._
import scala.language.reflectiveCalls

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Map[@sp(i) K, +V] extends PartialFunction[K, V] { self =>

  /**
   * Returns all key-value pairs stored in this map.
   * @return An enumerable sequence of key-value pairs.
   */
  def pairs: Enumerable[(K, V)]

  /**
   * Optionally retrieves the value associated with the specified key.
   * @param x The given key
   * @return The associated value. If the key is not found, return [[None]].
   */
  def ?(x: K): Option[V]

  /**
   * Retrieves the value associated with the specified key.
   * @param x The given key
   * @return The associated value
   * @throws NoSuchElementException if key not found
   */
  def apply(x: K): V

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
    def contains(x: K): Boolean = self.containsKey(x)
    def size: Int = self.size
    def elements: Enumerable[K] = self.pairs.map(_._1)
  }

  def keys = self.pairs.map(_._1)

  def values = self.pairs.map(_._2)

  // HELPER FUNCTIONS
  /**
   * Transforms the values of this map according to the specified function.
   *
   * WARNING: This function is equivalent to the Scala library's `mapValues`.
   * To transform all pairs in this map, use `this.pairs.map`.
   * @param f The specific function
   * @return A map view that maps every key of this map to `f(self(key))`.
   */
  def map[W](f: V => W): Map[K, W] = new Map[K, W] {
    def containsKey(x: K): Boolean = self.containsKey(x)
    def ?(x: K): Option[W] = (self ? x).map(f)
    def apply(x: K): W = f(self(x))
    def pairs = self.pairs.map { case (k, v) => (k, f(v)) }
    def size: Int = self.size
  }

  //TODO: ???
  def zip[W](that: Map[K, W]): Map[K, (V, W)] = new Map[K, (V, W)] {
    def apply(x: K): (V, W) = ???

    def ?(x: K): Option[(V, W)] = for {
      v ← self ? x
      w ← that ? x
    } yield (v, w)

    def pairs: Enumerable[(K, (V, W))] = ???

    def size: Int = ???

    def containsKey(x: K): Boolean = ???
  }

}

object Map {

  implicit def Functor[K]: Functor[({type λ[+V] = Map[K, V]})#λ] = new Functor[({type λ[+V] = Map[K, V]})#λ] {
    def map[X, Y](mx: Map[K, X])(f: X => Y): Map[K, Y] = mx map f
  }

}
