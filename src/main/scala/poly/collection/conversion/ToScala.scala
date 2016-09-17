package poly.collection.conversion

import poly.algebra.conversion.ToScala._
import poly.collection._
import poly.collection.mut._
import scala.{collection => sc}

/**
  * @author Tongfei Chen
  */
object ToScala {

  implicit class PolyIteratorAsScala[T](val e: Iterator[T]) extends AnyVal {

    /** Converts a Poly-collection iterator to a Scala iterator. */
    def asScalaIterator: sc.Iterator[T] = new sc.AbstractIterator[T] {
      private[this] var nextElem: T = default[T]
      private[this] var nextElemFetched: Boolean = false

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

    /** Converts a Poly-collection traversable to a Scala traversable. */
    def asScalaTraversable: sc.Traversable[T] = new sc.AbstractTraversable[T] {
      def foreach[U](f: T => U): Unit = xs.foreach(f)
    }
  }

  implicit class PolyIterableAsScala[T](val xs: Iterable[T]) extends AnyVal {

    /** Converts a Poly-collection iterable to a Scala iterable. */
    def asScalaIterable: sc.Iterable[T] = new sc.AbstractIterable[T] {
      def iterator: sc.Iterator[T] = xs.newIterator.asScalaIterator
    }
  }

  implicit class PolySeqAsScala[T](val xs: Seq[T]) extends AnyVal {

    /** Converts a Poly-collection sequence to a Scala sequence. */
    def asScalaSeq: sc.Seq[T] = new sc.AbstractSeq[T] {
      def length: Int = xs.length
      def apply(i: Int): T = xs(i)
      def iterator: sc.Iterator[T] = xs.newIterator.asScalaIterator
    }
  }

  implicit class PolyIndexedSeqAsScala[T](val xs: IndexedSeq[T]) extends AnyVal {

    /** Converts a Poly-collection indexed sequence to a Scala indexed sequence. */
    def asScalaIndexedSeq: sc.IndexedSeq[T] = new sc.AbstractSeq[T] with sc.IndexedSeq[T] {
      def length: Int = xs.fastLength
      def apply(i: Int): T = xs(i)
    }
  }

  implicit class PolySetAsScala[T](val xs: Set[T]) extends AnyVal {

    /** Converts a Poly-collection set to a Scala set. */
    def asScalaSet: sc.Set[T] = new sc.AbstractSet[T] {
      def contains(elem: T) = xs contains elem
      def +(elem: T) = AutoSet.from(xs.elements :+ elem)(xs.keyEq).asScalaSet
      def -(elem: T) = AutoSet.from(xs.elements.filter(x => xs.keyEq.ne(x, elem)))(xs.keyEq).asScalaSet
      def iterator = xs.elements.newIterator.asScalaIterator
    }

  }

  implicit class PolyMapAsScala[K, V](val xs: Map[K, V]) extends AnyVal {

    /** Converts a Poly-collection map to a Scala map. */
    def asScalaMap: sc.Map[K, V] = new sc.DefaultMap[K, V] {
      def get(key: K): Option[V] = xs ? key
      def iterator: sc.Iterator[(K, V)] = xs.pairs.newIterator.asScalaIterator
    }
  }

  /* //TODO: 0.2.0
  implicit class PolySortedSetAsScala[T](val xs: SortedSet[T]) extends AnyVal {
    def asScalaSortedSet: sc.SortedSet[T] = new sc.AbstractSet[T] with sc.SortedSet[T] {
      implicit def ordering = xs.keyOrder.asScalaOrdering
      def rangeImpl(from: Option[T], until: Option[T]) = ???
      def +(elem: T) = ???
      def contains(elem: T) = xs.contains(elem)
      def -(elem: T) = ???
      def keysIteratorFrom(start: T) = ???
      def iterator = xs.elements.newIterator.asScalaIterator
    }
  }
  */
}
