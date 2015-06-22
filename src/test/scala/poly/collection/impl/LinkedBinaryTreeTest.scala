package poly.collection.impl

import org.scalatest._
import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object LinkedBinaryTreeTest extends App {


    val T = new LinkedBinaryTree[Int]()
    T.addRoot(0)
    val n0 = T.rootNode
    val n1 = new T.Node(1, n0, null, null)
    val n2 = new T.Node(2, n0, null, null)
    val n3 = new T.Node(3, n1, null, null)
    val n4 = new T.Node(4, n1, null, null)
    val n5 = new T.Node(5, n2, null, null)
    val n6 = new T.Node(6, n2, null, null)
    val n7 = new T.Node(7, n4, null, null)
    val n8 = new T.Node(8, n4, null, null)
    val n9 = new T.Node(9, n8, null, null)
    n0.left = n1
    n0.right = n2
    n1.left = n3
    n1.right = n4
    n4.left = n7
    n4.right = n8
    n8.left = n9
    n2.left = n5
    n2.right = n6

    val o1 = T.preOrder.to[Seq]
    val o2 = T.inOrder.to[Seq]
    val o3 = T.postOrder.to[Seq]

    val breakpoint = 0

}
