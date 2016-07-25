package poly.collection.cache

import poly.collection._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait CachedFunction[A, B] extends Keyed[A] with (A => B) {
  /** Returns the cached values of this function. */
  def cache: Map[A, B]
  def eqOnKeys = cache.eqOnKeys
}
