package poly.collection.evidence

import scala.language.higherKinds

/**
 * This trait bundles two implicit evidences on one type into one evidence.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Ev2[E1[_], E2[_], T] extends Product2[E1[T], E2[T]] {

  def canEqual(that: Any) = false
}

object Ev2 {

  implicit def ev2[T, E1[_], E2[_]](implicit e1: E1[T], e2: E2[T]) = new Ev2[E1, E2, T] {
    def _1 = e1
    def _2 = e2
  }
}
