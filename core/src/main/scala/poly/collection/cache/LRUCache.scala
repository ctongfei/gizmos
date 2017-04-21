package poly.collection.cache

import poly.algebra._
import poly.collection._
import poly.collection.mut._

/**
 * An LRU cache of a function.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class LRUCache[K: Hashing, +R] private(f: K => R, n: Int) extends CachedFunction[K, R] {

  private[this] val c = LinkedHashMap[K, R]()

  def cache: Map[K, R] = c

  def apply(a: K) = {
    if (c containsKey a) { // cache hit
      val e = c.data.locate(a)
      e.prev.next = e.next // moves this entry to the MRU location
      e.next.prev = e.prev
      c.dummy.prev.next = e
      e.prev = c.dummy.prev
      e.next = c.dummy
      c.dummy.prev = e
      e.value
    }
    else { // cache miss
      if (c.size >= n)
        c.remove_!(c.dummy.next.key) // evict the LRU (least recently used) element
      val b = f(a) // heavyweight computation
      c.add_!(a, b) // caches the computation result
      b
    }
  }

  def clearCache_!() = c.clear_!()

}

object LRUCache {

  /**
   * Returns a function with an LRU cache. This is useful for wrapping a
   * high-cost pure function (e.g. reading from files).
   * @param n Capacity of this cache
   */
  def apply[K: Hashing, R](n: Int)(f: K => R) = new LRUCache(f, n)

  /**
   * Creates an LRU cache of a function using the default `hashCode` method on inputs
   * as the hashing function for the keys.
   */
  def byDefaultHashing[K, R](n: Int)(f: K => R) = new LRUCache(f, n)(Hashing.default[K])

  /**
   * Creates an LRU cache of a function using the reference (pointer) of the keys for hashing.
   */
  def byRefHashing[K <: AnyRef, R](n: Int)(f: K => R) = new LRUCache(f, n)(Hashing.byRef[K])

}
