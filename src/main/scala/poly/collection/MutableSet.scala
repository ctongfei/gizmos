package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutableSet[T] extends Set[T] {

  def add(x: T)

  def remove(x: T)

}
