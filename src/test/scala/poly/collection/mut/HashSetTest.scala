package poly.collection.mut

import poly.collection.impl._
import poly.util.typeclass.ops._
/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object HashSetTest extends App {

  val h = new HashTable[Int, Unit]()

  h.insert(1)
  h.insert(4)
  h.insert(8)
  h.insert(3)
  h.locate(-1).println
  h.locate(3).println
  h.remove(3)
  h.locate(3).println


}
