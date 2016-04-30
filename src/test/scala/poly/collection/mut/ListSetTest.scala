package poly.collection.mut

import poly.algebra.syntax._

/**
 * @author Tongfei Chen
 */
object ListSetTest extends App {

  val l = ListSet(1, 2, 3, 4, 5)
  l.addInplace(8)
  println(l.elements.buildString(" "))
  l.removeInplace(1)

  println(l.elements.buildString(" "))
  l.removeInplace(4)

  println(l.elements.buildString(" "))
  l.removeInplace(7)

  println(l.elements.buildString(" "))
  l.addInplace(3)

  println(l.elements.buildString(" "))
  l.addInplace(4)

  println(l.elements.buildString(" "))

}
