package poly.collection.impl.bst

import poly.algebra._
import poly.collection._
import poly.collection.impl._
import poly.collection.impl.bst.RedBlackTree._

/**
 * A red-black tree.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class RedBlackTree[K, V](implicit val order: WeakOrder[K]) extends StructureMutableMap[K, V]
{

  type Entry = RedBlackTreeEntry[K, V]

  val bst = new BinarySearchTree[Entry]()(WeakOrder.by((e: Entry) => e.key))

  override def size = bst.size

  def contains(x: K) = bst.locate(new Entry(x, default[V])) != null

  def newEnumerator = bst.inOrder.newEnumerator.map(e => e.key → e.value)

  private def fixAfterInsertion(_x: bst.Node) = {
    var x = _x
    x.data.color = red // ensure that a new node is labeled as red
    while (x != null && x != bst.rootNode && x.parent.data.color) {
      if (x.parent == x.parent.parent.left) {
        val y = x.parent.parent.right // uncle node of x
        if (y.data.color) {
          x.parent.data.color = black
          y.data.color = black
          x.parent.parent.data.color = red
          x = x.parent.parent
        } else {
          if (x == x.parent.right) {
            x = x.parent
            bst.rotateLeft(x)
          }
          x.parent.data.color = black
          x.parent.parent.data.color = red
          bst.rotateRight(x.parent.parent)
        }
      } else {
        val y = x.parent.parent.left
        if (y.data.color) {
          x.parent.data.color = black
          y.data.color = black
          x.parent.parent.data.color = red
          x = x.parent.parent
        } else {
          if (x == x.parent.left) {
            x = x.parent
            bst.rotateRight(x)
          }
          x.parent.data.color = black
          x.parent.parent.data.color = red
          bst.rotateLeft(x.parent.parent)
        }
      }
    }
    bst.rootNode.data.color = black
  }

  def add(key: K, value: V) = {
    if (bst.root == null) {
      bst.addRoot(new Entry(key, value, black)) // inserts a black node at the root position
    } else {
      val e = bst.add(new Entry(key, value, red)) // inserts an red node
      fixAfterInsertion(e)
    }
  }

  def remove(x: K) = ???

  def update(k: K, v: V) = ???

  def clear() = ???

  def get(x: K) = ???
}


object RedBlackTree {
  
  private[poly] type Color = Boolean
  private[poly] final val red = true
  private[poly] final val black = false
  
}

private[poly] class RedBlackTreeEntry[K, V](
  key: K,
  value: V,
  var color: Color = black
) extends KeyValuePair(key, value)
