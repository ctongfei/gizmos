package poly.collection.impl

import org.scalatest._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object LinkedBinaryTreeTest extends App {

    val T = new LinkedBinaryTree[Int]()
    T.addRoot(0)
    val n0 = T.rootNode
    val n1 = new LinkedBinaryTree.Node(1, T.dummy, T.dummy, parent = n0)
    val n2 = new LinkedBinaryTree.Node(2, T.dummy, T.dummy, parent = n0)
    val n3 = new LinkedBinaryTree.Node(3, T.dummy, T.dummy, parent = n1)
    val n4 = new LinkedBinaryTree.Node(4, T.dummy, T.dummy, parent = n1)
    val n5 = new LinkedBinaryTree.Node(5, T.dummy, T.dummy, parent = n2)
    val n6 = new LinkedBinaryTree.Node(6, T.dummy, T.dummy, parent = n2)
    val n7 = new LinkedBinaryTree.Node(7, T.dummy, T.dummy, parent = n4)
    val n8 = new LinkedBinaryTree.Node(8, T.dummy, T.dummy, parent = n4)
    val n9 = new LinkedBinaryTree.Node(9, T.dummy, T.dummy, parent = n8)
    n0.left = n1
    n0.right = n2
    n1.left = n3
    n1.right = n4
    n4.left = n7
    n4.right = n8
    n8.left = n9
    n2.left = n5
    n2.right = n6

    val (l :/ n \: r) = T

    val o1 = T.preOrder
    val o2 = T.inOrder
    val o3 = T.postOrder

    T.rootNode.depthFirstSearch(9) foreach println


    val breakpoint = 0

}
