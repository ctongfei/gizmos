package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.collection.mut._
import poly.collection.node.SinglePredNode

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 */

trait Fringe[S] extends Queue[S] {
  def topNode: SearchNode[S]
}

class DepthFirstFringe[S](start: S) extends Fringe[S] {
  val stack = ArrayStack[SearchNode[S]](SearchNode(start, SearchNode.dummy, 0))
  var currentNode: SearchNode[S] = SearchNode.dummy

  def push(x: S): Unit = stack push SearchNode(x, currentNode, currentNode.depth + 1)
  def pop(): S = {
    currentNode = stack.pop()
    currentNode.state
  }
  
  def size: Int = stack.size
  def top: S = currentNode.state
  def topNode: SearchNode[S] = currentNode
}