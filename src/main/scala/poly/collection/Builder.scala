package poly.collection

import poly.util.specgroup._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Builder[@sp(fdi) -T, +C] {
  def sizeHint(n: Int)
  def +=(x: T)
  def ++=(xs: Traversable[T]) = xs foreach +=
  def result: C
}
