package poly.collection

/**
 * Represents a sequence that is not empty.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait NonEmptySeq[+T] extends Seq[T] {

  override def isEmpty = false

}
