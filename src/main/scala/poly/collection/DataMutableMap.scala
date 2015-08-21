package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait DataMutableMap[K, V] extends Map[K, V] {
  def update(x: K, y: V): Unit
  def inplaceMap(f: V => V): Unit = ???
}
