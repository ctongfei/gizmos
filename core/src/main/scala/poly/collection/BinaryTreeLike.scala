package poly.collection

import poly.collection.exception._
import poly.collection.mut._
import poly.collection.node._

/**
 * @author Tongfei Chen
 */
trait BinaryTreeLike[+T, +N <: BinaryTreeNodeLike[T, N]] extends PartialFunction[Int, T] { self =>

  def dummy: N

  def rootNode: N

  def root: T = rootNode.data

  /** Returns the maximal height of this tree. */
  def height: Int = fold(0) { (l, r, _) => math.max(l, r) + 1 }

  /** Returns the number of nodes in this tree. */
  def size: Int = rootNode.preOrderTraversal.size

  def isEmpty = rootNode.isDummy

  /** Folds a binary tree bottom-up. This is analogous to the sequence `foldRight` in that both
   * are catamorphisms on recursive structures.
   */ // Deliberately implemented in a non-recursive manner to boost performance
  def fold[U](z: U)(f: (U, U, T) => U): U = {
    val s = ArrayStack[U]()
    for (n <- rootNode.postOrderTraversal) {
      if (n.leftNode .isDummy) s += z
      if (n.rightNode.isDummy) s += z
      val a = s.dequeue()
      val b = s.dequeue()
      s += f(a, b, n.data)
    }
    s.front
  }

  /**
   * Returns the ''i''th node of this binary tree.
   * The ordinal is defined as follows:
   *  <ul>
   *    <li> The index of the root node is 0; </li>
   *    <li> The left child of node ''i'' is 2''i'' + 1; </li>
   *    <li> The right child of node ''i'' is 2''i'' + 2. </li>
   * </ul>
   * @param i Index
   * @return The ''i''th node with the index defined above
   */
  def node(i: Int): N = {
    val x = i + 1
    var curr = dummy
    var mask = nextPowerOfTwo(x)
    if (mask > x) mask >>= 1

    while (mask != 0) {
      if ((x & mask) == 0)
        curr = curr.leftNode
      else curr = curr.rightNode
      if (curr.isDummy) return dummy
      mask >>= 1
    }
    curr
  }

  /**
   * '''Lazily''' traverses this binary tree in pre-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.preOrder == (a, b, d, e, c)
   *    └  d   e    ┘
   * }}}
   */
  def preOrder = rootNode.preOrderTraversal.map(_.data)

  /**
   * '''Lazily''' traverses this binary tree in in-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.inOrder == (d, b, e, a, c)
   *    └  d   e    ┘
   * }}}
   */
  def inOrder = rootNode.inOrderTraversal.map(_.data)

  /**
   * '''Lazily''' traverses this binary tree in post-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.postOrder = (d, e, b, c, a)
   *    └  d   e    ┘
   * }}}
   */
  def postOrder = rootNode.postOrderTraversal.map(_.data)

  /**
   * '''Lazily''' traverses this binary tree in level-order.
   * @example {{{
   *    ┌      a    ┐
   *    │     / \   │
   *    │    b   c  │
   *    │   / \     │.levelOrder == (a, b, c, d, e)
   *    └  d   e    ┘
   * }}}
   */
  def levelOrder = rootNode.breadthFirstTreeTraversal.map(_.data)

  def leafNodes = rootNode.preOrderTraversal.filter(_.isLeaf)

  def leaves = leafNodes.map(_.data)


  /** Returns the element on the ''i''th node of this binary tree. */
  def apply(i: Int): T = {
    val n = node(i)
    if (n.isDummy) throw new KeyNotFoundException(i)
    else n.data
  }

  def isDefinedAt(i: Int): Boolean = !(node(i).isDummy)

}
