package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class Alphabet[T] private(val w2i: HashMap[T, Int], val i2w: ArraySeq[T])
  extends BiMap[T, Int] with StructureMutableMap[T, Int]
{

  def apply(x: T): Int = w2i(x)
  def ?(x: T): Option[Int] = w2i.?(x)

  def invert(i: Int): T = i2w(i)
  def invertOption(i: Int): Option[T] = i2w.?(i)

  def containsKey(x: T): Boolean = w2i containsKey x
  def containsValue(v: Int): Boolean = i2w containsKey v

  def pairs: Enumerable[(T, Int)] = w2i.pairs

  def add(x: T, y: Int): Unit = ???
  def clear(): Unit = ???

  def remove(x: T): Unit = ???

  def update(x: T, y: Int): Unit = ???
}
