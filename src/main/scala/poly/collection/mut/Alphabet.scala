package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import scala.language.higherKinds

/**
 * Represents a mutable alphabet.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class Alphabet[T: Eq] private(
  private val w2i: KeyMutableMap[T, Int],
  private val i2w: ArraySeq[T])
  extends BijectiveMap[T, Int]
{

  def eqOnKeys = Eq[T]
  def eqOnValues = Eq[Int]

  def apply(x: T): Int = w2i ? x match {
    case Some(i) => i
    case None =>
      val newIndex = w2i.size
      w2i.addInplace(x, newIndex)
      i2w.appendInplace(x)
      newIndex
  }

  def ?(x: T): Option[Int] = w2i ? x

  override def size = i2w.size

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

  def apply[T: Eq] = new Alphabet(AutoMap[T, Int](), ArraySeq())

  /**
   * Creates an alphabet with a `nil` element. This element will be mapped to index `0`.
   */
  def withNil[T: Eq](nil: T) =
    new Alphabet[T](AutoMap[T, Int](nil → 0), ArraySeq(nil))

}
