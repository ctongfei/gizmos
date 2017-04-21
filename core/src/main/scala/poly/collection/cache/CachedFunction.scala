package poly.collection.cache

import poly.collection._

/**
 * Represents a function equipped with a cache: some of the argument/value pairs are cached in a map.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait CachedFunction[A, +B] extends Keyed[A] with Func[A, B] {

  /** Returns the cached argument/value map of this function. */
  def cache: Map[A, B]

  def keyEq = cache.keyEq
}
