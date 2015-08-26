package poly.collection.search

import poly.collection._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Fringe {

  def depthFirst[T]: Queue[T] = ArrayStack[T]()

  def breadthFirst[T]: Queue[T] = ArrayQueue[T]()


}
