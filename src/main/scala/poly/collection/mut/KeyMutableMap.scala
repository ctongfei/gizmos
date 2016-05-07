package poly.collection.mut

/**
 * @author Tongfei Chen
 */
trait KeyMutableMap[K, V] extends ValueMutableMap[K, V] { self =>

  /**
   * Adds a key-value pair into this map.
   * If the key exists, the corresponding value is updated to the given value.
   */
  def addInplace(k: K, v: V): Unit

  def addInplace(kv: (K, V)): Unit = addInplace(kv._1, kv._2)

  def getOrElseUpdate(k: K, v: => V) = {
    if (notContainsKey(k))
      self.addInplace(k, v)
    self(k)
  }

  /**
   * Removes a key-value pair given the key.
   * If the key does not exist in the map, this method does nothing.
   * @param x Key
   */
  def removeInplace(x: K): Unit

  /** Removes all key-value pairs in this map. */
  def clear(): Unit

}
