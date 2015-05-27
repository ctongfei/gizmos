package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class TreeNode[T](
  var data: T,
  var parent: TreeNode[T] = null,
  var children: Seq[TreeNode[T]] = null
) extends BidiNode[T] {

  def ancestors = ListSeq(parent)
  def descendants = children

}
