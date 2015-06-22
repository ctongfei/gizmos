package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion._
import poly.util.specgroup._
import scala.reflect._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait CollectionFactoryWithOrder[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[@sp(fdi) T: ClassTag: WeakOrder]: Builder[T, C[T]]

  /** Creates an empty collection. */
  def empty[@sp(fdi) T: ClassTag: WeakOrder]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[@sp(fdi) T: ClassTag: WeakOrder](xs: T*): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[@sp(fdi) T: ClassTag: WeakOrder](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  implicit def factory: CollectionFactoryWithOrder[C] = this

}
