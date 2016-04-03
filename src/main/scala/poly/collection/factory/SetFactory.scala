package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
  * Generic factory for companion objects of set types.
 *
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait SetFactory[S[_]] {

  implicit def newBuilder[K: Equiv]: Builder[K, S[K]]

  def empty[K: Equiv]: S[K] = newBuilder[K].result

  def apply[K: Equiv](ks: K*): S[K] = {
    val b = newBuilder[K]
    b.sizeHint(ks.length)
    b addAllInplace ks
    b.result
  }

  def from[K: Equiv](xs: Traversable[K]): S[K] = {
    val b = newBuilder[K]
    b addAllInplace xs
    b.result
  }

}

trait SortedSetFactory[S[_]] {

  implicit def newBuilder[K: WeakOrder]: Builder[K, S[K]]

  def empty[K: WeakOrder]: S[K] = newBuilder[K].result

  def apply[K: WeakOrder](ks: K*): S[K] = {
    val b = newBuilder[K]
    b.sizeHint(ks.length)
    b addAllInplace ks
    b.result
  }

  def from[K: WeakOrder](xs: Traversable[K]): S[K] = {
    val b = newBuilder[K]
    b addAllInplace xs
    b.result
  }

}


trait HashSetFactory[S[_]] {

  implicit def newBuilder[K: IntHashing]: Builder[K, S[K]]

  def empty[K: IntHashing]: S[K] = newBuilder[K].result

  def apply[K: IntHashing](ks: K*): S[K] = {
    val b = newBuilder[K]
    b.sizeHint(ks.length)
    b addAllInplace ks
    b.result
  }

  def from[K: IntHashing](xs: Traversable[K]): S[K] = {
    val b = newBuilder[K]
    b addAllInplace xs
    b.result
  }

}