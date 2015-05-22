import poly.collection.impl._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object BinaryHeapTest extends App {

  val h = BinaryHeap(5, 3, 1, 3, 5, 5, 8, 1, 10)

  val r1 = h.dequeue()
  assert(r1 == 1)
  val r2 = h.dequeue()
  assert(r2 == 1)
  assert(h.size == 7)

  h.enqueue(0)
  assert(h.front == 0)

  print("x")

}
