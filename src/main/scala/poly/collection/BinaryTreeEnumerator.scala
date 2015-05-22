package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BinaryTreeEnumerator[T] {
  def current: T
  def advanceLeft(): Unit
  def advanceRight(): Unit
}
