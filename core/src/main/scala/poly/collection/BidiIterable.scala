package poly.collection

/**
 * Represents a collection that can be iterated in two directions, namely forward and backward.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BidiIterable[+T] extends Iterable[T] { self =>

  import BidiIterable._

  /** Returns an iterator that iterates through this collection in reverse order. */
  def newReverseIterator: Iterator[T]

  /** Returns the reverse of this iterable collection. $LAZY */
  override def reverse: BidiIterable[T] = new AbstractBidiIterable[T] {
    def newReverseIterator = self.newIterator
    def newIterator = self.newReverseIterator
    override def reverse = self
  }

  override def map[U](f: T => U): BidiIterable[U] = new AbstractBidiIterable[U] {
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

  override def last = reverse.head

  def asBidiIterable = new AbstractBidiIterable[T] {
    def newReverseIterator = self.newReverseIterator
    def newIterator = self.newIterator
  }

}

object BidiIterable {

  def ofIterator[T](forward: => Iterator[T], backward: => Iterator[T]): BidiIterable[T] = new BidiIterable[T] {
    def newReverseIterator = backward
    def newIterator = forward
  }

}

abstract class AbstractBidiIterable[+T] extends AbstractIterable[T] with BidiIterable[T]
