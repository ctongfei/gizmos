package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion._
import poly.util.specgroup._
import scala.language.higherKinds
import scala.reflect._

trait CollectionFactoryWithTag[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[@sp(fdi) T: ClassTag]: Builder[T, C[T]]

  /** Creates an empty collection. */
  def empty[T: ClassTag]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: ClassTag](xs: T*): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[@sp(fdi) T: ClassTag](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  implicit def factory: CollectionFactoryWithTag[C] = this

}






