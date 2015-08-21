package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait RemovableBuilder[-T, +C] extends Builder[T, C] {

  def -=(x: T): Unit

  def --=(xs: Traversable[T]) = xs foreach -=

}
