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
trait CollectionFactoryWithAdditiveGroup[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T: ClassTag: AdditiveGroup]: Builder[T, C[T]]

  /** Creates an empty collection. */
  def empty[T: ClassTag: AdditiveGroup]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: ClassTag: AdditiveGroup](xs: T*): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: ClassTag: AdditiveGroup](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  implicit def factory: CollectionFactoryWithAdditiveGroup[C] = this

}
