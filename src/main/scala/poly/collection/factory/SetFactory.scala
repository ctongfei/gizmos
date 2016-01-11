package poly.collection.factory

import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
  * @author Tongfei Chen
  */
trait SetFactory[S[_]] {

  implicit def newBuilder[K]: Builder[K, S[K]]

  def apply[K](ks: K*): S[K] = {
    val b = newBuilder[K]
    b.sizeHint(ks.length)
    b addAll ks
    b.result
  }

}
