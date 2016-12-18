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
  private[this] val w2i: KeyMutableMap[T, Int],
  private[this] val i2w: KeyMutableMap[Int, T])
  extends BiMap[T, Int]
{

  private[this] var frozen = false

  def keySet = w2i.keySet
  def valueSet = i2w.keySet

  def apply(x: T): Int = synchronized {
    if (frozen) return w2i ? x getOrElse 0
    w2i ? x match {
      case Some(i) => i
      case None =>
        val newIndex = w2i.size
        w2i += (x, newIndex)
        i2w += (newIndex, x)
        newIndex
    }
  }

  def ?(x: T): Option[Int] = w2i ? x

  override def size = i2w.size

  def invert(i: Int) = i2w(i)
  def invertOption(i: Int) = i2w ? i

  def freeze() = synchronized {
    frozen = true
  }

  def unfreeze() = synchronized {
    frozen = false
  }

  override def pairs = i2w.pairs.map(_.swap) // normally faster than traversing through i2w, and is ordered

  def clear_!(): Unit = {
    w2i.clear_!()
    i2w.clear_!()
  }

}

object Alphabet {

  /**
   * Creates an alphabet with a `nil` element. This element will be mapped to index `0`.
   */
  def withNil[T: Eq](nil: T) =
    new Alphabet[T](
      AutoMap[T, Int](nil -> 0),
      DenseIntKeyedMap(0 -> nil)
    )

}
