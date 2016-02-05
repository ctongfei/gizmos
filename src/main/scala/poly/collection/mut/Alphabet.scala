package poly.collection.mut

import poly.algebra._
import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
class Alphabet[T: IntHashing] private(
  private val w2i: HashMap[T, Int],
  private val i2w: ArraySeq[T])
  extends BijectiveMap[T, Int]
{

  def equivOnValue = Equiv.default[Int]

  def apply(x: T): Int = w2i ? x match {
    case Some(i) => i
    case None =>
      val newIndex = w2i.size
      w2i.add(x, newIndex)
      i2w.appendInplace(x)
      newIndex
  }

  def ?(x: T): Option[Int] = w2i ? x

  def equivOnKey = implicitly[IntHashing[T]]

  override def size = w2i.size

  def invert(i: Int): T = i2w(i)
  def invertOption(i: Int): Option[T] = i2w ? i

  def containsKey(x: T): Boolean = w2i containsKey x
  def containsValue(v: Int): Boolean = i2w containsKey v

  def pairs: Iterable[(T, Int)] = w2i.pairs

  def clear(): Unit = {
    w2i.clear()
    i2w.clear()
  }


}

object Alphabet {


}