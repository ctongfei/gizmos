package poly.collection

/**
 * Represents a binary tree.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BinaryTree[+T] extends (Int =!> T) {

  /** Returns the maximal depth of this tree. */
  def depth: Int

  /** Returns the number of nodes in this tree. */
  def size: Int

  /**
   * Returns the ''i''th node of this binary tree.
   * The ''i''th is defined as follows:
   *  - The index of the root node is 0;
   *  - The left child of node ''i'' is 2''i'' + 1;
   *  - The right child of node ''i'' is 2''i'' + 2.
   * @param i Index
   * @return The ''i''th node with the index defined above
   */
  def apply(i: Int): T

}
