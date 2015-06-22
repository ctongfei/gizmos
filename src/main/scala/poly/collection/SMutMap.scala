package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait DMutMap[K, V] extends Map[K, V] {
  def update(x: K, y: V): Unit
  def inplaceMapValues(f: V => V): Unit
}

trait SMutMap[K, V] extends DMutMap[K, V] {

  def add(x: K, y: V): Unit

  def add(xy: (K, V)): Unit = add(xy._1, xy._2)

  def remove(x: K): Unit

  def clear(): Unit

}
