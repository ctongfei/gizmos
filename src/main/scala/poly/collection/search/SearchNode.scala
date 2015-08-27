package poly.collection.search

import poly.algebra._
import poly.algebra.implicits._
import poly.collection.exception._
import poly.collection.node._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SearchNode[+S] extends SinglePredNode[S] {
  def depth: Int
  def state: S
  def parent: SearchNode[S]
  def data = state
}

object SearchNode {
  def apply[S](s: S, d: Int, p: SearchNode[S]) = new SearchNode[S] {
    val depth = d
    val state = s
    val parent = p
  }
  object dummy extends SearchNode[Nothing] {
    def depth = -1
    def state = throw new NoSuchElementException
    def parent = this
    override def isDummy = true
  }
}
