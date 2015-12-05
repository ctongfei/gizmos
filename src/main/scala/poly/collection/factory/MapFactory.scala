package poly.collection.factory

import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.Scala._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MapFactory[M[_, _]] {

  implicit def newBuilder[K, V]: Builder[(K, V), M[K, V]]

  def apply[K, V](kvs: (K, V)*): M[K, V] = {
    val b = newBuilder[K, V]
    b.sizeHint(kvs.length)
    b ++= kvs
    b.result
  }

}
