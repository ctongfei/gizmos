package poly.collection.factory

import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.Scala._
import scala.language.higherKinds

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
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
