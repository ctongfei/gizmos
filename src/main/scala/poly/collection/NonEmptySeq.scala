package poly.collection

/**
 * Represents a sequence that is not empty.
 * This trait enforces non-emptiness of a sequence
 * (thus relaxing the monoid requirements on fold operations to semigroups).
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait NonEmptySeq[+T] extends Seq[T] {

  override def isEmpty = false

}
