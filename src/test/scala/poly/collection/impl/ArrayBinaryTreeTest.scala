package poly.collection.impl

import poly.collection.mut._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ArrayBinaryTreeTest extends App {

  val F = false
  val T = true

  val d = ResizableArray(0, 1, 2, 3, 4, 5, 6, 0, 0, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9)
  val e = ResizableArray(T, T, T, T, T, T, T, F, F, T, T, F, F, F, F, F, F, F, F, F, F, T)

  val B = new ArrayBinaryTree[Int](d, e)

  val o1 = B.preOrder.to[ArraySeq]
  print(o1)
  val o2 = B.inOrder.to[ArraySeq]
  print(o2)
  val o3 = B.postOrder.to[ArraySeq]
  print(o3)

  val breakpoint = 0

}
