package poly.collection.impl

import org.scalatest._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object LinkedBinaryTreeTest extends App {


    val T = new LinkedBinaryTree[Int]()
    T.addRoot(0)
    val n0 = T.root
    val n1 = new LinkedBinaryTree.Node(1, n0, T.dummy, T.dummy)
    val n2 = new LinkedBinaryTree.Node(2, n0, T.dummy, T.dummy)
    val n3 = new LinkedBinaryTree.Node(3, n1, T.dummy, T.dummy)
    val n4 = new LinkedBinaryTree.Node(4, n1, T.dummy, T.dummy)
    val n5 = new LinkedBinaryTree.Node(5, n2, T.dummy, T.dummy)
    val n6 = new LinkedBinaryTree.Node(6, n2, T.dummy, T.dummy)
    val n7 = new LinkedBinaryTree.Node(7, n4, T.dummy, T.dummy)
    val n8 = new LinkedBinaryTree.Node(8, n4, T.dummy, T.dummy)
    val n9 = new LinkedBinaryTree.Node(9, n8, T.dummy, T.dummy)
    n0.left = n1
    n0.right = n2
    n1.left = n3
    n1.right = n4
    n4.left = n7
    n4.right = n8
    n8.left = n9
    n2.left = n5
    n2.right = n6

    val o1 = T.preOrder(n0).map(_.data).to[ArraySeq]
    val o2 = T.inOrder(n0).map(_.data).to[ArraySeq]
    val o3 = T.postOrder(n0).map(_.data).to[ArraySeq]

    val breakpoint = 0

}
