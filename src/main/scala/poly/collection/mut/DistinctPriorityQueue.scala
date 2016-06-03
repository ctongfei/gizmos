package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
class DistinctPriorityQueue[K: Eq, T] private(private val inner: PriorityQueue[T], val keySelector: T => K)
  extends KeyedPriorityQueue[K, T] {

  private[this] val seen = AutoMap[K, T]()

  def keyElementMap: Map[K, T] = seen

  def eqOnKeys = Eq[K]

  implicit def orderOnElements = inner.orderOnElements

  def push(x: T) = {
    val k = keySelector(x)
    if (seen notContainsKey k) { // node not seen
      inner += x
      seen += (k, x)
    } else if (x < seen(k)) { // node is better than what is seen, update
      inner += x
      seen(k) = x
    } else { /* node is worse than what is seen, discard this node, do nothing */ }
  }

  def top = inner.top
  
  def pop() = inner.pop()

  def elements = inner.elements

}

object DistinctPriorityQueue {
  //TODO: is this the right way to do this?
  def apply[K: Eq, T](inner: PriorityQueue[T], keySelector: T => K) = new DistinctPriorityQueue(inner, keySelector)
}
