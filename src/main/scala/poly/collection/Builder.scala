package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Builder[-T, +C] {
  def +=(x: T)
  def ++=(xs: Traversable[T]) = xs foreach +=
  def result: C
}

trait CollectionBuilder[T, C[_]] extends Builder[T, C[T]] {
  def sizeHint(n: Int)
}
