package poly.collection.conversion

import poly.collection._
import poly.collection.node._
import scala.{collection => sc}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 */
object Scala {

  implicit def scalaTraversableAsPoly[T](xs: sc.Traversable[T]): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U) = xs.foreach(f)
  }

  implicit def scalaIteratorAsPoly[T](xs: sc.Iterator[T]): Iterator[T] = new Iterator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }

  implicit def scalaIterableAsPoly[T](xs: sc.Iterable[T]): Iterable[T] = new AbstractIterable[T] {
    def newIterator = scalaIteratorAsPoly[T](xs.iterator)
  }

  implicit def scalaSeqAsPoly[T](xs: sc.Seq[T]): Seq[T] = new Seq[T] {
    class WrappedNode(val s: sc.Seq[T], override val isDummy: Boolean = false) extends SeqNode[T] {
      def data = s.head
      def next = {
        val t = s.tail
        new WrappedNode(t, t.isEmpty)
      }
    }
    override def newIterator = scalaIteratorAsPoly[T](xs.iterator)
    def dummy = new SeqNode[T] {
      def data = throw new NoSuchElementException
      def next = new WrappedNode(xs.view, xs.isEmpty)
      def isDummy = true
    }
    override def apply(i: Int) = xs(i)
    override def length = xs.length
  }

  implicit def scalaIndexedSeqAsPoly[T](xs: sc.IndexedSeq[T]): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int) = xs.apply(i)
    def fastLength = xs.length
  }



  implicit class PolyIteratorAsScala[T](val e: Iterator[T]) extends AnyVal {

    /**
     * Converts a Poly-collection enumerator to a Scala iterator.
     * @return An equivalent Scala iterator.
     */
    def asScalaIterator: sc.Iterator[T] = new sc.AbstractIterator[T] {
      var nextElem: T = default[T]
      var nextElemFetched: Boolean = false

      def hasNext: Boolean = {
        if (nextElemFetched) true
        else {
          if (e.advance()) {
            nextElem = e.current
            nextElemFetched = true
            true
          }
          else false
        }
      }

      def next() = {
        if (nextElemFetched) {
          nextElemFetched = false
          nextElem
        }
        else {
          e.advance()
          e.current
        }
      }
    }
  }

  implicit class PolyTraversableAsScala[T](val xs: Traversable[T]) extends AnyVal {
    def asScalaTraversable: sc.Traversable[T] = new sc.AbstractTraversable[T] {
      def foreach[U](f: T => U): Unit = xs.foreach(f)
    }
  }

  implicit class PolyIterableAsScala[T](val xs: Iterable[T]) extends AnyVal {
    def asScalaIterable: sc.Iterable[T] = new sc.AbstractIterable[T] {
      def iterator: sc.Iterator[T] = xs.newIterator.asScalaIterator
    }
  }

  implicit class PolySeqAsScala[T](val xs: Seq[T]) extends AnyVal {
    def asScalaSeq: sc.Seq[T] = new sc.AbstractSeq[T] {
      def length: Int = xs.length
      def apply(i: Int): T = xs(i)
      def iterator: sc.Iterator[T] = xs.newIterator.asScalaIterator
    }
  }

  implicit class PolyIndexedSeqAsScala[T](val xs: IndexedSeq[T]) extends AnyVal {
    def asScalaIndexedSeq: sc.IndexedSeq[T] = new sc.IndexedSeq[T] {
      def length: Int = xs.fastLength
      def apply(i: Int): T = xs(i)
    }
  }

  implicit class PolyMapAsScala[K, V](val xs: Map[K, V]) extends AnyVal {
    def asScalaMap: sc.Map[K, V] = new sc.DefaultMap[K, V] {
      def get(key: K): Option[V] = xs ? key
      def iterator: sc.Iterator[(K, V)] = xs.pairs.newIterator.asScalaIterator
    }
  }


}
