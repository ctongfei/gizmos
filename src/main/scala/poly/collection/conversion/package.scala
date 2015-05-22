package poly.collection

import java.{util => ju}
import java.{lang => jl}
import scala.{collection => sc}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
package object conversion {

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


  implicit class polyEnumeratorAsScala[T](val xs: Enumerator[T]) extends AnyVal {

    /**
     * Converts a Poly-collection enumerator to a Scala iterator.
     * @return An equivalent Scala iterator.
     */
    def asScala: sc.Iterator[T] = new sc.Iterator[T] {
      var nextElem: T = default[T]
      var nextElemFetched: Boolean = false

      def hasNext: Boolean = {
        if (nextElemFetched) true
        else {
          if (xs.advance()) {
            nextElem = xs.current
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
          xs.advance()
          xs.current
        }
      }
    }
  }

}
