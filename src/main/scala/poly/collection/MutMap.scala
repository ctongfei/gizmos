package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutMap[K, V] extends Map[K, V] {

  def update(x: K, y: V): Unit

  def insert(x: K, y: V): Unit

  def remove(x: K): Unit

  def clear(): Unit

  def inplaceMapValues(f: V => V): Unit

}
