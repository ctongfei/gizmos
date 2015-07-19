package poly.collection.mut

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListSetTest extends App {

  val l = ListSet(1, 2, 3, 4, 5)
  l.add(8)
  println(l.buildString(" "))
  l.remove(1)

  println(l.buildString(" "))
  l.remove(4)

  println(l.buildString(" "))
  l.remove(7)

  println(l.buildString(" "))
  l.add(3)

  println(l.buildString(" "))
  l.add(4)

  println(l.buildString(" "))

}
