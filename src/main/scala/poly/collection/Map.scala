package poly.collection

import poly.algebra.hkt._
import poly.collection.exception._
import poly.util.specgroup._
import scala.language.reflectiveCalls

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Map[@sp(i) K, +V] extends PartialFunction[K, V] { self =>

  def pairs: Enumerable[(K, V)]

  def applyOption(x: K): Option[V]

  def apply(x: K): V

  def contains(x: K): Boolean

  def getOrElse[W >: V](x: K, default: => W) = applyOption(x) match {
    case Some(y) => y
    case None => default
  }

  def isDefinedAt(x: K) = contains(x)

  def keys = self.pairs.map(_._1)

  def values = self.pairs.map(_._2)

  /**
   * Transforms the values of this map according to the specified function.
   *
   * WARNING: This function is equivalent to the Scala library's `mapValues`.
   * To transform all pairs in this map, use `this.pairs.map`.
   * @param f The specific function
   * @return A map view that maps every key of this map to `f(self(key))`.
   */
  def map[W](f: V => W): Map[K, W] = new Map[K, W] {
    def contains(x: K): Boolean = self.contains(x)
    def applyOption(x: K): Option[W] = self.applyOption(x).map(f)
    def apply(x: K): W = f(self(x))
    def pairs = self.pairs.map { case (k, v) => (k, f(v)) }
  }

}

object Map {

  implicit def Functor[K]: Functor[({type λ[+V] = Map[K, V]})#λ] = new Functor[({type λ[+V] = Map[K, V]})#λ] {
    def map[X, Y](mx: Map[K, X])(f: X => Y): Map[K, Y] = mx map f
  }

}
