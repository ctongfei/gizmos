package poly.collection.search.node

import poly.collection.exception._
import poly.collection.node._
import poly.collection.search._

/**
  * @author Tongfei Chen
  */
trait WithParent[S] extends NodeWithParent[S] {
  def state: S
  def parent: WithParent[S]
  def depth: Int
  def data = state
}

object WithParent {

  def dummy[S]: WithParent[S] = new WithParent[S] {
    def state = throw new DummyNodeException
    def parent = this
    def isDummy = true
    def depth = -1
  }

  def apply[S](s: S, d: Int, p: WithParent[S]) = new WithParent[S] {
    def state = s
    def parent = p
    def isDummy = false
    def depth = d
  }

  implicit def SearchNodeInfo[S]: SearchNodeInfo[WithParent[S], S] = new SearchNodeInfo[WithParent[S], S] {
    def startNode(s: S) = WithParent(s, 0, dummy[S])
    def state(n: WithParent[S]) = n.state
    def nextNode(currNode: WithParent[S])(nextState: S) = WithParent(nextState, currNode.depth + 1, currNode)
  }

}
