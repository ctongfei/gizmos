package poly.collection

import poly.algebra._
import poly.algebra.syntax._

/**
 * @author Tongfei Chen
 */ // TODO: EXPERIMENTAL!
trait Foldable[+T] { self =>

  def foldMap[U](f: T => U)(z: U)(op: (U, U) => U): U

  def foldMapByMonoid[U : Monoid](f: T => U) = foldMap(f)(id)(_ op _)



}
