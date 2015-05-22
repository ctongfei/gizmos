import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ArrayQueueTest {

  def main(args: Array[String]): Unit = {
    val a = ArrayQueue[Int](1, 2)
    val r1 = a.dequeue()
    a.enqueue(3)
    a.enqueue(4)
    a.enqueue(5)
    a.enqueue(6)
    val r2 = a.dequeue()
    val r3 = a.dequeue()
    val r4 = a.dequeue()
    val r5 = a.dequeue()
    a.enqueue(7)
    a.enqueue(8)
    a.enqueue(9)
    a.enqueue(0)
  }

}
