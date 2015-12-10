package poly.collection.search

import poly.algebra._
import poly.algebra.implicits._
import poly.collection.exception._
import poly.collection.node._

/**
  * Witnesses that a type can be used as the type of weighted search node for a specific state type.
  * @tparam N Type of search node
  * @tparam S Type of search state
  * @tparam C Type of path cost
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.1.0
  */
trait WeightedSearchNodeInfo[N, S, C] {
  /** Creates a node given the starting state. */
  def startNode(s: S): N

  /** Gets the state from a node. */
  def state(n: N): S

  /** Given the current node and the next state, returns a new node that wraps around the next state. */
  def nextNode(currNode: N)(nextState: S, cost: C): N
}

