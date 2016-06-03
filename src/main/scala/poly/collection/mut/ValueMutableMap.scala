package poly.collection.mut

import poly.collection._

/**
 * Represents a map whose values can be mutated.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait ValueMutableMap[K, V] extends Map[K, V] {

  /** Sets the value associated with the specific key. */
  def update(k: K, v: V): Unit

  /** Transforms the value of this map in-place given a specific function. */
  def mapInplace(f: V => V): Unit = for (k <- this.keys) this.update(k, f(this(k)))
}
