package poly.collection.mut

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyMutableMultimap[K, V] {

  def addInplace(k: K, v: V): Unit

  def addInplace(kv: (K, V)): Unit = addInplace(kv._1, kv._2)

  def removeInplace(k: K, v: V): Unit

  def removeAllInplace(k: K): Unit

  def clear(): Unit

  final def +=(k: K, v: V) = addInplace(k, v)
  final def +=(kv: (K, V)) = addInplace(kv)
  final def -=(k: K, v: V) = removeInplace(k, v)
  final def -=(k: K) = removeAllInplace(k)

}
