package poly.collection

/**
 * Represents an indexed binary tree (e.g. an array binary tree)
 * where fast random access using its canonical index is possible.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait IndexedBinaryTree[+T] extends BinaryTree[T] {
  def fastApply(i: Int): T
  final override def apply(i: Int) = fastApply(i)
}

