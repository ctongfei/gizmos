package poly.collection.search

import poly.collection._
import poly.collection.exception._
import poly.collection.mut._

/**
 * Defines a space of search spaces.
 * @author Yuhuan Jiang (jyuhuan@gmail.com).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpace[S] {

  def succ(x: S): Enumerable[S]

  def depthFirstTreeTraversal(start: S): Enumerable[S] =
    Enumerable.ofEnumerator(new DepthFirstTreeEnumerator[S](start)(this))

  def depthFirstTreeSearch(start: S, goal: S => Boolean): Seq[S] = {
    val traversal = depthFirstTreeTraversal(start).takeWhile(s => !goal(s))



    val e = new DepthFirstTreeEnumerator[S](start)(this)

    case class State(state: S, prev: State)
    val stack = ArrayStack(State(start, null))
    while (stack.notEmpty) {
      val curr = stack.pop()
      if (!goal(curr.state))
        stack ++= succ(curr.state).map(s => State(s, curr))
      else {
        return Enumerable.iterate(curr)(s => s.prev).takeWhile(s => s.prev != null).map(s => s.state).to[Seq]
      }
    }
    throw new GoalNotFoundException(goal)
  }

  def breadthFirstTreeSearch(initial: S, goal: S => Boolean): Seq[S] = ???
}
