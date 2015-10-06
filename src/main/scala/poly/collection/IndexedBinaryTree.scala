package poly.collection

/**
 * Represents an indexed binary tree (e.g. an array binary tree)
 * where fast random access is possible for its canonical index.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait IndexedBinaryTree[+T] extends BinaryTree[T] {
  def fastApply(i: Int): T
  override def apply(i: Int) = fastApply(i)
}

