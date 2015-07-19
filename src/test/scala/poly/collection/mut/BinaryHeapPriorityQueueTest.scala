package poly.collection.mut

import org.scalatest._
import poly.algebra.implicits._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class BinaryHeapPriorityQueueTest extends FunSuite {

  val h = BinaryHeapPriorityQueue(5, 3, 1, 3, 5, 5, 8, 1, 10)

  val r1 = h.pop()
  assert(r1 == 1)
  val r2 = h.pop()
  assert(r2 == 1)
  assert(h.size == 7)

  h.push(0)
  assert(h.top == 0)

  print("x")

}
