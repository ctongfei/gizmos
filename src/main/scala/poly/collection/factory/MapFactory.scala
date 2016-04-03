package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait MapFactory[M[_, _]] {

  implicit def newBuilder[K: Equiv, V]: Builder[(K, V), M[K, V]]

  def apply[K: Equiv, V](kvs: (K, V)*): M[K, V] = {
    val b = newBuilder[K, V]
    b.sizeHint(kvs.length)
    b addAllInplace kvs
    b.result
  }

}

trait MapFactoryWithIntHashing[M[_, _]] {

  implicit def newBuilder[K: IntHashing, V]: Builder[(K, V), M[K, V]]

  def apply[K: IntHashing, V](kvs: (K, V)*): M[K, V] = {
    val b = newBuilder[K, V]
    b.sizeHint(kvs.length)
    b addAllInplace kvs
    b.result
  }

}

trait MapFactoryWithOrder[M[_, _]] {

  implicit def newBuilder[K: WeakOrder, V]: Builder[(K, V), M[K, V]]

  def apply[K: WeakOrder, V](kvs: (K, V)*): M[K, V] = {
    val b = newBuilder[K, V]
    b.sizeHint(kvs.length)
    b addAllInplace kvs
    b.result
  }

}