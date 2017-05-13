package poly.collection

trait RangeGroupQueryable[T] extends RangeMonoidQueryable[T] {

  def group: Group[T]

  def monoid = group

}
