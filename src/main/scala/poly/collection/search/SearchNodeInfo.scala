package poly.collection.search

import poly.algebra._
import poly.algebra.implicits._
import poly.collection.exception._
import poly.collection.node._

/**
 * Witnesses that a type can be used as the type of search node for a specific state type.
 * @tparam N Type of search node
 * @tparam S Type of search state
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait SearchNodeInfo[N, S] {
  /** Creates a node given the starting state. */
  def startNode(s: S): N

  /** Gets the state from a node. */
  def state(n: N): S

  /** Given the current node and the next state, returns a new node that wraps around the next state. */
  def nextNode(currNode: N)(nextState: S): N
}

object SearchNodeInfo {
  def None[S]: SearchNodeInfo[S, S] = new SearchNodeInfo[S, S] {
    def startNode(s: S) = s
    def state(n: S) = n
    def nextNode(currNode: S)(nextState: S) = nextState
  }
}
