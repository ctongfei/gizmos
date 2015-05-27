package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */

trait CollectionFactory[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T]: CollectionBuilder[T, C]

  /** Creates an empty collection. */
  def empty[T]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T](xs: T*): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  implicit def factory: CollectionFactory[C] = this

}

trait TaggedCollectionFactory[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T: ClassTag]: CollectionBuilder[T, C]

  /** Creates an empty collection. */
  def empty[T: ClassTag]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: ClassTag](xs: T*): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: ClassTag](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  implicit def factory: TaggedCollectionFactory[C] = this

}


trait OrderedCollectionFactory[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T: ClassTag: WeakOrder]: CollectionBuilder[T, C]

  /** Creates an empty collection. */
  def empty[T: ClassTag: WeakOrder]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: ClassTag: WeakOrder](xs: T*): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: ClassTag: WeakOrder](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b ++= xs
    b.result
  }

  implicit def factory: OrderedCollectionFactory[C] = this

}


trait AdditiveGroupCollectionFactory[C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T: ClassTag: AdditiveGroup]: CollectionBuilder[T, C]

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

  implicit def factory: AdditiveGroupCollectionFactory[C] = this

}
