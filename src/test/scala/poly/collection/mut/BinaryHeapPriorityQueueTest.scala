package poly.collection.mut

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object BinaryHeapPriorityQueueTest extends App {

  val h = BinaryHeapPriorityQueue(5, 3, 1, 3, 5, 5, 8, 1, 10)

  val r1 = h.pop()
  assert(r1 == 1)
  val r2 = h.pop()
  assert(r2 == 1)
  assert(h.size == 7)

  h.push(0)
  assert(h.front == 0)

  print("x")

}
