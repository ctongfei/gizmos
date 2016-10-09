package poly.collection

/**
 * Represents a sequence whose elements are sorted.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait SortedSeq[T] extends Seq[T] with SortedIterable[T] { self =>

  override def filter(f: T => Boolean): SortedSeq[T] = super[Seq].filter(f).asIfSorted(self.elementOrder)

  override def filterNot(f: T => Boolean) = filter(x => !f(x))

  //TODO: thenSortBy
}

abstract class AbstractSortedSeq[T] extends AbstractSeq[T] with SortedSeq[T]
