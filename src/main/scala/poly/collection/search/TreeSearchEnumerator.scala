package poly.collection.search

import poly.collection._
import poly.collection.mut._
import poly.collection.node._

/**
 * An enumerator that executes a search on trees (assumes that there's no loop).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class TreeSearchEnumerator[S](
  val start: S,
  val fringe: Queue[SearchNode[S]]
)(implicit ss: StateSpace[S]) extends Enumerator[S] {

  private[this] var curr: SearchNode[S] = SearchNode.dummy

  fringe += SearchNode(start, 0, curr)

  def current = curr.state

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succ(curr.data).map(s => SearchNode(s, curr.depth + 1, curr))
      true
    }
    else false
  }

  def currentNode = curr

}

class DepthFirstTreeSearchEnumerator[S](start: S)(implicit ss: StateSpace[S])
  extends TreeSearchEnumerator[S](start, ArrayStack[SearchNode[S]]())(ss)

class BreadthFirstTreeSearchEnumerator[S](start: S)(implicit ss: StateSpace[S])
  extends TreeSearchEnumerator[S](start, ArrayQueue[SearchNode[S]]())(ss)
