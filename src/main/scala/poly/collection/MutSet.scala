package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutSet[T] extends Set[T] {

  def add(x: T)

  def remove(x: T)

}
