package poly.collection.node

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait TreeNodeLike[+T, +N <: TreeNodeLike[T, N]] extends ForwardNodeLike[T, N] { self: N =>

  def children: Iterable[N]

  def succ = children

  def isLeaf = children.isEmpty

  def notLeaf = !isLeaf

  def preOrder = self.depthFirstTreeTraversal

  def levelOrder = self.breadthFirstTreeTraversal

  def postOrder: Iterable[N] = ???

}

trait TreeNode[+T] extends ForwardNode[T] with TreeNodeLike[T, TreeNode[T]]

object TreeNode {

  object Dummy extends TreeNode[Nothing] {
    def children = Iterable.Empty
    def data = throw new NoSuchElementException
    def isDummy = true
  }

}