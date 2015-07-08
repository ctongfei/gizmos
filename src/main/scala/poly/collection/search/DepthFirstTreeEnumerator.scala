package poly.collection.search

import poly.collection._
import poly.collection.mut._

/**
 * An enumerator that executes depth first search on trees (assumes that there's no loop).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class DepthFirstTreeEnumerator[S](initial: S)(implicit ss: StateSpace[S]) extends Enumerator[S] {

  case class State(state: S, prev: State)

  val fringe = ArrayStack(State(initial, null))
  private var curr: State = null

  def current = curr.state

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succ(curr.state).map(s => State(s, curr))
      true
    }
    else false
  }
  
}
