package poly.collection.mut

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListMapTest extends App {

  val m = ListMap[Int, String](1 → "a", 2 → "b", 3 → "c", 4 → "d")

  m.add(5 → "e")
  println(m.buildString(" "))

  m.get(6)

  m.remove(3)
  println(m.buildString(" "))



}
