package poly.collection.mut

/**
 * @author Tongfei Chen
 */
trait KeyMutableMap[K, V] extends ValueMutableMap[K, V] {

  /**
   * Adds a key-value pair into this map.
   * If the key exists, the corresponding value is updated to the given value.
   * @param x Key
   * @param y Value
   */
  def add(x: K, y: V): Unit

  def add(xy: (K, V)): Unit = add(xy._1, xy._2)

  /**
   * Removes a key-value pair given the key.
   * If the key does not exist in the map, this method does nothing.
   * @param x Key
   */
  def remove(x: K): Unit

  /** Removes all key-value pairs in this map. */
  def clear(): Unit

}
