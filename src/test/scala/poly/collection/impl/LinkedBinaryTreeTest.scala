package poly.collection.impl

import org.scalatest._
import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object LinkedBinaryTreeTest extends App {


    val T = new LinkedBinaryTree[Int]()
    T.addRoot(0)
    val n0 = T.rootNode
    val n1 = new T.Node(1, parent = n0)
    val n2 = new T.Node(2, parent = n0)
    val n3 = new T.Node(3, parent = n1)
    val n4 = new T.Node(4, parent = n1)
    val n5 = new T.Node(5, parent = n2)
    val n6 = new T.Node(6, parent = n2)
    val n7 = new T.Node(7, parent = n4)
    val n8 = new T.Node(8, parent = n4)
    val n9 = new T.Node(9, parent = n8)
    n0.left = n1
    n0.right = n2
    n1.left = n3
    n1.right = n4
    n4.left = n7
    n4.right = n8
    n8.left = n9
    n2.left = n5
    n2.right = n6

    val o1 = T.preOrder.to[ArraySeq]
    val o2 = T.inOrder.to[ArraySeq]
    val o3 = T.postOrder.to[ArraySeq]

    val breakpoint = 0

}
