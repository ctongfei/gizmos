package poly.collection.node

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
trait BiTreeNode[+T] extends BiNode[T] with TreeNode[T] with NodeWithParent[T] { self =>

  override def isDummy = false

  def parent: BiTreeNode[T]
  override def pred = ListSeq(parent).filter(_.notDummy)
  def children: Seq[BiTreeNode[T]]
  override def succ = children

  override def map[U](f: T => U): BiTreeNode[U] = new BiTreeNode[U] {
    def children = self.children.map(_.map(f))
    def parent = self.parent.map(f)
    def data = f(self.data)
  }

}
