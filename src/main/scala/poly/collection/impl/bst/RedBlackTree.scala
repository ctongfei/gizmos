package poly.collection.impl.bst

import poly.algebra._
import poly.collection._
import poly.collection.impl._

/**
 * A red-black tree.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class RedBlackTree[K, V](implicit val order: WeakOrder[K]) extends StructureMutableMap[K, V] {

  class RbEntry(
    override val key: K,
    override var value: V = default[V],
    var red: Boolean = false
  ) extends KeyValuePair(key, value)

  val bst = new BinarySearchTree[RbEntry]

  override def size = bst.size

  def contains(x: K) = bst.locate(new RbEntry(x)) != null

  def newEnumerator = bst.inOrder.newEnumerator.map(e => e.key ¡ú e.value)

  private def fixAfterInsertion(_x: bst.Node) = {
    var x = _x
    x.data.red = true // ensure that a new node is labeled as red
    while (x != null && x != bst.rootNode && x.parent.data.red) {
      if (x.parent == x.parent.parent.left) {
        val y = x.parent.parent.right // uncle node of x
        if (y.data.red) {
          x.parent.data.red = false
          y.data.red = false
          x.parent.parent.data.red = true
          x = x.parent.parent
        } else {
          if (x == x.parent.right) {
            x = x.parent
            bst.rotateLeft(x)
          }
          x.parent.data.red = false
          x.parent.parent.data.red = true
          bst.rotateRight(x.parent.parent)
        }
      } else {
        val y = x.parent.parent.left
        if (y.data.red) {
          x.parent.data.red = false
          y.data.red = false
          x.parent.parent.data.red = true
          x = x.parent.parent
        } else {
          if (x == x.parent.left) {
            x = x.parent
            bst.rotateRight(x)
          }
          x.parent.data.red = false
          x.parent.parent.data.red = true
          bst.rotateLeft(x.parent.parent)
        }
      }
    }
    bst.rootNode.data.red = false
  }

  def add(key: K, value: V) = {
    if (bst.root == null) {
      bst.addRoot(new RbEntry(key, value, false)) // inserts a black node at the root position
    } else {
      val e = bst.add(new RbEntry(key, value, true)) // inserts an red node
      fixAfterInsertion(e)
    }
  }

  def remove(x: K) = ???

  def update(k: K, v: V) = ???
}
