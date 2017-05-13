package poly.collection

import poly.algebra._

trait RangeGroupQueryable[T] extends RangeMonoidQueryable[T] {

  def group: Group[T]

  def monoid = group

}
