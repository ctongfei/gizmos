package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Multiset[+T] extends Enumerable[T] {

  /** Returns the multiplicity (number of occurrence) of an element in this multiset. */
  def multiplicity[U >: T](x: U): Int

  /** Tests if an element belongs to this set. */
  def contains[U >: T](x: U): Boolean

  /** Returns the union of two sets. */
  def |[U >: T](that: Multiset[U]): Multiset[U]

  /** Returns the intersection of two sets. */
  def &[U >: T](that: Multiset[U]): Multiset[U]

  /** Returns the difference of two sets. */
  def \[U >: T](that: Multiset[U]): Multiset[U]

  /** Tests if this set is a subset of another set. */
  def <=[U >: T](that: Multiset[U]): Boolean

  /** Tests if this set is a strict subset of another set. */
  def <[U >: T](that: Multiset[U]): Boolean

  /** Tests if this set is a strict superset of another set. */
  def >[U >: T](that: Multiset[U]): Boolean = that < this

  /** Tests if this set is a superset of another set. */
  def >=[U >: T](that: Multiset[U]): Boolean = that <= this
}
