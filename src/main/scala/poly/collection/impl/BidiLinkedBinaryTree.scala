package poly.collection.impl

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BidiLinkedBinaryTree[@specialized(Int, Double) T] {

  type Node = BidiLinkedBinaryTree.Node[T]

  val dummy = new Node(default[T])
  var root: Node = null
  var size = 0
  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  def addRoot(x: T) = {
    root = new Node(x, dummy, dummy, dummy)
    dummy.left = root
    dummy.right = root
    size = 1
  }

  /** Locates the binary tree node at the given index. */
  def locate(i: Int): Node = {
    var x = i + 1
    var curr = dummy
    var depth = 1
    while (depth <= x) {
      x / depth match {
        case 0 => curr = curr.left
        case 1 => curr = curr.right
      }
      x %= depth
      depth <<= 1
    }
    curr
  }

  def apply(i: Int) = locate(i).data

  def update(i: Int, x: T) = locate(i).data = x

  /**
   * Performs pre-order traversal from a node.
   * @param n Starting node
   * @return A non-strict sequence of the traversal.
   */
  def preOrder(n: Node): Enumerable[Node] = new Enumerable[Node] {
    def enumerator: Enumerator[Node] = new Enumerator[Node] {
      private val s = ArrayStack[Node](n)
      private var curr: Node = null
      def advance(): Boolean = {
        if (s.isEmpty) return false
        curr = s.pop()
        if (curr.right != null) s.push(curr.right)
        if (curr.left != null) s.push(curr.left)
        true
      }
      def current: Node = curr
    }
  }


}

object BidiLinkedBinaryTree {

  class Node[@specialized(Int, Double) T] (
    var data: T,
    var parent: Node[T] = null,
    var left: Node[T] = null,
    var right: Node[T] = null
  ) extends BidiNode[T] {

    def ancestors = ListSeq(parent)
    def descendants = ListSeq(left, right)

  }


}