package poly.collection

/**
 * Represents a collection that can be iterated in both forward and backward directions.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BidiIterable[+T] extends Iterable[T] { self =>

  /** Returns an iterator that iterates through this collection in reverse order. */
  def newReverseIterator: Iterator[T]

  /** $LAZY Reverses this iterable collection. */
  override def reverse: BidiIterable[T] = new AbstractBidiIterable[T] {
    def newReverseIterator = self.newIterator
    def newIterator = self.newReverseIterator
    override def reverse = self
  }

  override def map[U](f: T => U): BidiIterable[U] = new BidiIterableT.Mapped(self, f)

  override def last = reverse.head

  def asBidiIterable: BidiIterable[T] = new BidiIterableT.Bare[T](self)

}

object BidiIterable {

  def ofIterator[T](forward: => Iterator[T], backward: => Iterator[T]): BidiIterable[T] = new BidiIterable[T] {
    def newReverseIterator = backward
    def newIterator = forward
  }

}

abstract class AbstractBidiIterable[+T] extends AbstractIterable[T] with BidiIterable[T]

private[poly] object BidiIterableT {

  class Mapped[T, U](self: BidiIterable[T], f: T => U) extends AbstractBidiIterable[U] {
    def newIterator = new AbstractIterator[U] {
      private[this] val i = self.newIterator
      def current = f(i.current)
      def advance() = i.advance()
    }

    def newReverseIterator = new AbstractIterator[U] {
      private[this] val i = self.newReverseIterator
      def current = f(i.current)
      def advance() = i.advance()
    }

    override def size = self.size // map preserves size
    override def sizeKnown = self.sizeKnown
  }

  class Bare[+T](self: BidiIterable[T]) extends AbstractBidiIterable[T] {
    def newReverseIterator = self.newReverseIterator
    def newIterator = self.newIterator
  }

}