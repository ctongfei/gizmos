package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen
 */
trait BidiTree[+T] extends Tree[T] { self =>
  def rootNode: BidiTreeNode[T]
}
