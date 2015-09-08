package poly.collection.node

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait TreeNode[+T] extends ForwardNode[T] { self =>

  def children: Seq[TreeNode[T]]

  def succ = children

  override def map[U](f: T => U): TreeNode[U] = new TreeNode[U] {
    def children = self.children.map(_.map(f))
    def data = f(self.data)
    def isDummy = self.isDummy
  }

}
