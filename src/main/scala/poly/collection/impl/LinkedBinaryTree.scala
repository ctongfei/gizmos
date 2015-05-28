package poly.collection.impl

import poly.collection._
import poly.collection.exception._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class LinkedBinaryTree[@specialized(Int, Double) T] {

  type Node = LinkedBinaryTree.Node[T]

  val dummy = new Node(default[T])
  var root: Node = null
  var size = 0
  dummy.left = dummy
  dummy.right = dummy
  dummy.parent = dummy

  def addRoot(x: T) = {
    root = new Node(x, dummy, dummy, dummy)
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
   * @return A non-strict sequence of the pre-order traversal.
   */
  def preOrder(n: Node): Enumerable[Node] = Enumerable.ofEnumerator {
    new Enumerator[Node] {
      private val s = ArrayStack[Node](n)
      private var curr: Node = null
      def advance(): Boolean = {
        if (s.isEmpty) return false
        curr = s.pop()
        if (curr.right ne dummy) s.push(curr.right)
        if (curr.left ne dummy) s.push(curr.left)
        true
      }
      def current: Node = curr
    }
  }

  /**
   * Performs in-order traversal from a node.
   * @param n Starting node
   * @return A non-strict sequence of the in-order traversal.
   */
  def inOrder(n: Node): Enumerable[Node] = Enumerable.ofEnumerator {
    new Enumerator[Node] {
      private[this] val s = ArrayStack[Node]()
      private[this] var v: Node = null
      private[this] var curr: Node = null
      pushLeft(n)

      private[this] def pushLeft(n: Node) = {
        var node = n
        while (node ne dummy) {
          s.push(node)
          node = node.left
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.pop()
        curr = v
        v = v.right
        if (v ne dummy) pushLeft(v)
        true
      }
      def current: Node = curr
    }
  }

  /**
   * Performs post-order traversal from a node.
   * @param n Starting node
   * @return A non-strict sequence of the post-order traversal.
   */
  def postOrder(n: Node): Enumerable[Node] = Enumerable.ofEnumerator {
    new Enumerator[Node] {
      private val s = ArrayStack[Node]()
      private[this] var v: Node = null
      private[this] var curr: Node = null
      pushLeft(n)

      private[this] def pushLeft(n: Node) = {
        var node = n
        while (node ne dummy) {
          s.push(node)
          node = node.left
        }
      }
      def advance(): Boolean = {
        if (s.isEmpty) return false
        v = s.pop()
        curr = v
        if (v.parent.left eq v) {
          v = v.parent.right
          if (v ne dummy) pushLeft(v)
        }
        true
      }
      def current: Node = curr
    }
  }


}

object LinkedBinaryTree {

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