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

  def breadthFirstTreeTraversal(start: S): Enumerable[S] =
    Enumerable.ofEnumerator(new BreadthFirstTreeEnumerator[S](start)(this))

  def depthFirstTreeSearch(start: S, goal: S => Boolean): Seq[S] = {
    val dfs = new DepthFirstTreeEnumerator(start)(this)
    dfs.takeWhile(s => !goal(s)).foreach(s => ())
    val goalNode = dfs.fringe.top
    if (!goal(goalNode.state)) throw new GoalNotFoundException(goal)
    Enumerable.iterate(goalNode)(s => s.prev)
      .takeWhile(s => s.prev != null)
      .map(s => s.state).to[ArraySeq]
  }

  def breadthFirstTreeSearch(start: S, goal: S => Boolean): Seq[S] = {
    val bfs = new BreadthFirstTreeEnumerator(start)(this)
    bfs.takeWhile(s => !goal(s)).foreach(s => ())
    val goalNode = bfs.fringe.top
    if (!goal(goalNode.state)) throw new GoalNotFoundException(goal)
    Enumerable.iterate(goalNode)(s => s.prev)
      .takeWhile(s => s.prev != null)
      .map(s => s.state).to[ArraySeq]
  }
}
