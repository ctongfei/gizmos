package poly.collection.factory

import poly.collection._
import poly.collection.conversion._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MapFactory[M[_, _]] {

  implicit def newBuilder[K, V]: Builder[(K, V), M[K, V]]

  def apply[K, V](kvs: (K, V)*): M[K, V] = {
    val b = newBuilder[K, V]
    b ++= kvs
    b.result
  }

}
