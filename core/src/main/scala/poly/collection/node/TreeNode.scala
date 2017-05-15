package poly.collection.node

import poly.collection._
import poly.collection.mut._

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

  def postOrder: Iterable[N] = Iterable.ofIterator {
    new AbstractIterator[N] {
      private[this] val s = ArrayStack[(N, Boolean)]()
      private[this] var prevNode = default[N]
      private[this] var prevLast = false
      private[this] var curr = default[N]
      s += (self, true)
      def current = curr
      def advance(): Boolean = {
        while (s.notEmpty) {
          val (c, l) = s.front
          if (prevLast) {
            curr = c
            s.dequeue()
            if (l) prevNode = c; prevLast = l; // a subtree is completely traversed if l == true
            return true
          } else {
            val children = c.children to ListSeq
            if (children.isEmpty) {
              curr = c
              s.dequeue()
              if (l) prevNode = c; prevLast = l;
              return true
            }
            else {
              s ++= (children.init.map {_ -> false}) :+ (children.last -> true)
            }
          }
        }
        false
      }
    }
  }

}

trait TreeNode[+T] extends ForwardNode[T] with TreeNodeLike[T, TreeNode[T]]

object TreeNode {

  object Dummy extends TreeNode[Nothing] {
    def children = Iterable.empty
    def data = throw new NoSuchElementException
    def isDummy = true
  }

}