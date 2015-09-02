package poly.collection.node

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BiTreeNode[+T] extends TreeNode[T] with SinglePredNode[T] { self =>

  override def isDummy = false
  override def notDummy = !isDummy

  def parent: BiTreeNode[T]
  def children: Iterable[BiTreeNode[T]]
  override def succ = children

  override def map[U](f: T => U): BiTreeNode[U] = new BiTreeNode[U] {
    def children = self.children.map(_.map(f))
    def parent = self.parent.map(f)
    def data = f(self.data)
  }

  override def reverse: SeqNode[T] with BackwardNode[T] = new SeqNode[T] with BackwardNode[T] {
    override def isDummy = self.isDummy
    override def notDummy = !isDummy
    def next = self.parent.reverse
    def pred = self.succ.map(_.reverse)
    def data = self.data
    override def reverse = self
  }

}
