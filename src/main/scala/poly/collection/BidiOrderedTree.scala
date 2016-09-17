package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen
 */
trait BidiOrderedTree[+T] extends BidiTree[T] with OrderedTree[T] { self =>
  def rootNode: BidiOrderedTreeNode[T]

}
