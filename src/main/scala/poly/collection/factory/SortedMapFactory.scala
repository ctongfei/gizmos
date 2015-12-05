package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.Scala._
import scala.language.higherKinds

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
trait SortedMapFactory[M[_, _]] {

  implicit def newBuilder[K, V](implicit K: WeakOrder[K]): Builder[(K, V), M[K, V]]

  def apply[K, V](kvs: (K, V)*)(implicit K: WeakOrder[K]): M[K, V] = {
    val b = newBuilder[K, V](K)
    b.sizeHint(kvs.length)
    b ++= kvs
    b.result
  }

}
