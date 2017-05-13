package poly.collection.mut

import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

import scala.util._

/**
 * Represents a random queue in which the front element is random (uniformly distributed among all elements in the queue).
 * @author Tongfei Chen
 * @since 0.1.0
 */
class RandomQueue[T] private(val data: ResizableSeq[T]) extends Queue[T] {

  private def randomIndex = Random.nextInt(data.len)

  def elements = data.asIndexedSeq

  def enqueue(x: T) = data.append_!(x)

  def front: T = data(randomIndex)

  def dequeue(): T = {
    val i = randomIndex
    data.swap_!(i, data.len - 1)
    val t = data(data.len - 1)
    data.len -= 1
    t
  }
}

object RandomQueue extends SeqFactory[RandomQueue] {
  def newSeqBuilder[T]: Builder[T, RandomQueue[T]] = new Builder[T, RandomQueue[T]] {
    private[this] val data = new ResizableSeq[T]()
    def add(x: T) = data.append_!(x)
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def result() = new RandomQueue(data)
  }
}
