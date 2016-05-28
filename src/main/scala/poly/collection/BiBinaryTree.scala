package poly.collection

import poly.collection.node._

/**
 * Represents a binary in which the parent of each node can be efficiently retrieved.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BiBinaryTree[+T] extends BinaryTree[T] { self ⇒

  def rootNode: BiBinaryTreeNode[T]

}
