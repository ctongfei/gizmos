package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class Alphabet[T] private(val w2i: HashMap[T, Int], val i2w: ArraySeq[T])
  extends BiMap[T, Int] with StructureMutableMap[T, Int]
{

  def apply(x: T): Int = w2i(x)
  def applyOption(x: T): Option[Int] = w2i.applyOption(x)

  def invert(i: Int): T = i2w(i)
  def invertOption(i: Int): Option[T] = i2w.applyOption(i)

  def contains(x: T): Boolean = w2i contains x
  def containsValue(v: Int): Boolean = i2w contains v

  def pairs: Enumerable[(T, Int)] = w2i.pairs

  def add(x: T, y: Int): Unit = ???
  def clear(): Unit = ???

  def remove(x: T): Unit = ???

  def update(x: T, y: Int): Unit = ???
}
