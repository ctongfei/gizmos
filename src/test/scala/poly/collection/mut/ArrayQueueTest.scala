package poly.collection.mut

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ArrayQueueTest {

  def main(args: Array[String]): Unit = {
    val a = ArrayQueue[Int](1, 2)
    val r1 = a.pop()
    a.push(3)
    a.push(4)
    a.push(5)
    a.push(6)
    val r2 = a.pop()
    val r3 = a.pop()
    val r4 = a.pop()
    val r5 = a.pop()
    a.push(7)
    a.push(8)
    a.push(9)
    a.push(0)
  }

}
