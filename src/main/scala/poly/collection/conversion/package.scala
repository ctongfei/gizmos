package poly.collection

import java.{util => ju}
import java.{lang => jl}
import scala.{collection => sc, _}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
package object conversion {

  //region Scala/Java to Poly

  implicit def scalaTraversableAsPoly[T](xs: sc.Traversable[T]): Traversable[T] = new Traversable[T] {
    def foreach[U](f: T => U) = xs.foreach(f)
  }
  implicit def scalaIteratorAsPoly[T](xs: sc.Iterator[T]): Enumerator[T] = new Enumerator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }
  implicit def scalaIterableAsPoly[T](xs: sc.Iterable[T]): Enumerable[T] = new Enumerable[T] {
    def enumerator = scalaIteratorAsPoly[T](xs.iterator)
  }
  implicit def scalaSeqAsPoly[T](xs: sc.Seq[T]): Seq[T] = new Seq[T] {
    def enumerator = scalaIteratorAsPoly[T](xs.iterator)
    def apply(i: Int) = xs(i)
    def length = xs.length
  }


  implicit def javaIterableAsPoly[T](xs: jl.Iterable[T]): Enumerable[T] = new Enumerable[T] {
    def enumerator = javaIteratorAsPoly[T](xs.iterator())
  }
  implicit def javaIteratorAsPoly[T](xs: ju.Iterator[T]): Enumerator[T] = new Enumerator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }
  implicit def javaListAsPoly[T](xs: ju.List[T]): IndexedSeq[T] = new IndexedSeq[T] {
    def length = xs.size
    def apply(i: Int) = xs.get(i)
  }
  //endregion

  //region Poly to Scala

  implicit class PolyEnumeratorAsScala[T](val e: Enumerator[T]) extends AnyVal {

    /**
     * Converts a Poly-collection enumerator to a Scala iterator.
     * @return An equivalent Scala iterator.
     */
    def asScalaIterator: sc.Iterator[T] = new sc.Iterator[T] {
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
    def asScalaTraversable: sc.Traversable[T] = new sc.Traversable[T] {
      def foreach[U](f: (T) => U): Unit = xs.foreach(f)
    }
  }

  implicit class PolyEnumerableAsScala[T](val xs: Enumerable[T]) extends AnyVal {
    def asScalaIterable: sc.Iterable[T] = new Iterable[T] {
      def iterator: Iterator[T] = xs.enumerator.asScalaIterator
    }
  }

  implicit class PolySeqAsScala[T](val xs: Seq[T]) extends AnyVal {
    def asScalaSeq: sc.Seq[T] = new sc.Seq[T] {
      def length: Int = xs.length
      def apply(i: Int): T = xs(i)
      def iterator: Iterator[T] = xs.enumerator.asScalaIterator
    }
  }

  implicit class PolyIndexedSeqAsScala[T](val xs: IndexedSeq[T]) extends AnyVal {
    def asScalaIndexedSeq: sc.IndexedSeq[T] = new sc.IndexedSeq[T] {
      def length: Int = xs.length
      def apply(i: Int): T = xs(i)
    }
  }


  //endregion

}
