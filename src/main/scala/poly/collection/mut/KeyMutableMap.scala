package poly.collection.mut

import poly.algebra._
import poly.collection._

/**
 * Represents a mutable map whose key-value pairs can be added or removed given a key.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyMutableMap[K, V] extends ValueMutableMap[K, V] { self =>

  /**
   * Adds a key-value pair into this map.
   * If the key exists, the corresponding value would be updated to the given value.
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
   * @param k Key
   */
  def removeInplace(k: K): Unit

  /** Removes all key-value pairs in this map. */
  def clear(): Unit

  /**
   * Wraps the keys of this mutable map with a bijection.
   * @example {{{
   *  val map = {1 -> 'A', 2 -> 'B'} contramap {"apple" <-> 1, "orange" <-> 2, "peach" <-> 3}
   *  map += "peach" -> 'C'
   * }}}
   */
  override def contramap[J](f: Bijection[J, K]): KeyMutableMap[J, V] = new KeyMutableMapT.Contramapped(self, f)

  /**
   * Wraps around this map and modified its behavior:
   * When an absent key is accessed, returns the given default value. But this key would not be added to the map.
   */
  def withDefault(default: => V): KeyMutableMap[K, V] = new KeyMutableMapT.WithDefault(self, default)

  /**
   * Wraps around this map and modified its behavior:
   * When an absent key is accessed, the given default value will be added to the map, associating with the accessed key.
   * @example {{{
   *   val map: HashMap[K, ArraySeq[V]]() withDefaultUpdate ArraySeq[V]()
   *   map(k) :+= v
   * }}}
   */
  def withDefaultUpdate(default: => V): KeyMutableMap[K, V] = new KeyMutableMapT.WithDefaultUpdate(self, default)

  final def +=(k: K, v: V) = addInplace(k, v)
  final def +=(kv: (K, V)) = addInplace(kv)
  final def -=(k: K) = removeInplace(k)
}

private[poly] object KeyMutableMapT {

  class Contramapped[K, V, J](self: KeyMutableMap[K, V], f: Bijection[J, K])
    extends MapT.Contramapped[K, V, J](self, f) with KeyMutableMap[J, V]
  {
    def addInplace(k: J, v: V) = self.addInplace(f(k), v)
    def removeInplace(k: J) = self.removeInplace(f(k))
    def clear() = self.clear()
    def update(k: J, v: V) = self(f(k)) = v
  }

  class WithDefault[K, V](self: KeyMutableMap[K, V], default: => V)
    extends MapT.WithDefault[K, V, V](self, default) with KeyMutableMap[K, V]
  {
    def addInplace(k: K, v: V) = self.addInplace(k, v)
    def removeInplace(k: K) = self.removeInplace(k)
    def clear() = self.clear()
    def update(k: K, v: V) = self.update(k, v)
  }

  class WithDefaultUpdate[K, V](self: KeyMutableMap[K, V], default: => V) extends KeyMutableMap[K, V] {
    def addInplace(k: K, v: V) = self.addInplace(k, v)
    def removeInplace(k: K) = self.removeInplace(k)
    def clear() = self.clear()
    def update(k: K, v: V) = {
      if (self.containsKey(k)) self.update(k, v)
      else self.addInplace(k, v)
    }

    def keySet = self.keySet
    override def pairs = self.pairs
    def apply(k: K) = (this ? k) getOrElse {
      self.addInplace(k, default)
      self(k) //TODO
    }
    def ?(k: K) = self ? k
}

}
