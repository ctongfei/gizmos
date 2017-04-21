package poly.collection.factory

import poly.algebra._
import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait SetFactory[S[_], Ev[_]] extends Factory1[Id, S, Ev] {

  def newSetBuilder[K: Ev]: Builder[K, S[K]]

  def newBuilder[K: Ev] = newSetBuilder[K]

}
