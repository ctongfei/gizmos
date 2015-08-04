package poly.collection.search

import poly.collection._
import poly.collection.mut._

import scala.collection._

/**
 * An enumerator that executes depth first search on trees (assumes that there's no loop).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class DepthFirstTreeEnumerator[S](start: S)(implicit ss: StateSpace[S]) extends Enumerator[S] {

  case class SearchState(state: S, prev: SearchState)

  val fringe = ArrayStack(SearchState(start, null))
  private var curr: SearchState = null

  def current = curr.state

  def advance() = {
    if (fringe.notEmpty) {
      curr = fringe.pop()
      fringe ++= ss.succ(curr.state).map(s => SearchState(s, curr))
      true
    }
    else false
  }
  
}
