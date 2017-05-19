package poly.collection.typeclass

import scala.language.higherKinds

/**
 * Combines two evidences into one.
 */ //TODO: to be changed into [[cats.data.Tuple2K]].
class Ev2[E1[_], E2[_], T](val _1: E1[T], val _2: E2[T]) extends Product2[E1[T], E2[T]] {
  def canEqual(that: Any) = false
}

object Ev2 {
  implicit def ev2[T, E1[_], E2[_]](implicit e1: E1[T], e2: E2[T]) = new Ev2[E1, E2, T](e1, e2)
}
