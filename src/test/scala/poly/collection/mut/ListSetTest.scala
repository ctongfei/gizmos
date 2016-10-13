package poly.collection.mut

import poly.algebra.syntax._

/**
 * @author Tongfei Chen
 */
object ListSetTest extends App {

  val l = ListSet(1, 2, 3, 4, 5)
  l.add_!(8)
  println(l.elements.buildString(" "))
  l.remove_!(1)

  println(l.elements.buildString(" "))
  l.remove_!(4)

  println(l.elements.buildString(" "))
  l.remove_!(7)

  println(l.elements.buildString(" "))
  l.add_!(3)

  println(l.elements.buildString(" "))
  l.add_!(4)

  println(l.elements.buildString(" "))

}
