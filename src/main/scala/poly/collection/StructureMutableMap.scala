package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait ImmutableMap[K, V] extends Map[K, V]

trait DataMutableMap[K, V] extends Map[K, V] {
  def update(x: K, y: V): Unit
  def inplaceMap(f: V => V): Unit = ???
}

trait StructureMutableMap[K, V] extends DataMutableMap[K, V] {

  def add(x: K, y: V): Unit

  def add(xy: (K, V)): Unit = add(xy._1, xy._2)

  def remove(x: K): Unit

  def clear(): Unit

}
