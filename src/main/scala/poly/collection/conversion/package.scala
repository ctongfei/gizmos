package poly.collection

import poly.collection._
import poly.collection.conversion._
import poly.collection.node._

import java.{util => ju}
import java.{lang => jl}
import scala.{collection => sc}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
package object conversion {

  //region Scala/Java to Poly

  implicit def scalaTraversableAsPoly[T](xs: sc.Traversable[T]): Traversable[T] = new AbstractTraversable[T] {
    def foreach[U](f: T => U) = xs.foreach(f)
  }
  implicit def scalaIteratorAsPoly[T](xs: sc.Iterator[T]): Iterator[T] = new AbstractIterator[T] {
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
      def next = new WrappedNode(s.tail, s.tail.isEmpty)
    }
    override def newIterator = scalaIteratorAsPoly[T](xs.iterator)
    def headNode = new WrappedNode(xs, xs.isEmpty)
    override def apply(i: Int) = xs(i)
    override def length = xs.length
  }
  implicit def scalaIndexedSeqAsPoly[T](xs: sc.IndexedSeq[T]): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int) = xs.apply(i)
    def fastLength = xs.length
  }


  implicit def javaIterableAsPoly[T](xs: jl.Iterable[T]): Iterable[T] = new AbstractIterable[T] {
    def newIterator = javaIteratorAsPoly[T](xs.iterator())
  }
  implicit def javaIteratorAsPoly[T](xs: ju.Iterator[T]): Iterator[T] = new AbstractIterator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }
  implicit def javaListAsPoly[T](xs: ju.List[T]): IndexedSeq[T] = new DataMutableIndexedSeq[T] {
    def fastLength = xs.size
    def fastApply(i: Int) = xs.get(i)
    def update(i: Int, x: T) = xs.set(i, x)
  }
  implicit def javaMapAsPoly[K, V](jm: ju.Map[K, V]): Map[K, V] = new KeyMutableMap[K, V] {
    def add(x: K, y: V): Unit = jm.put(x, y)
    def clear(): Unit = jm.clear()
    def remove(x: K): Unit = jm.remove(x)
    def update(x: K, y: V): Unit = jm.put(x, y)
    def ?(x: K): Option[V] = Option(jm.get(x))
    def pairs: Iterable[(K, V)] = jm.entrySet.map(e => e.getKey → e.getValue)
    def size = jm.size
    def apply(x: K): V = jm.get(x)
    def containsKey(x: K): Boolean = jm.containsKey(x)
  }
  //endregion

  //region Poly to Scala

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

  implicit class PolyEnumerableAsScala[T](val xs: Iterable[T]) extends AnyVal {
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

  implicit class PolyLinearSeqAsScala[T](val xs: Seq[T]) extends AnyVal {
    def asScalaLinearSeq: sc.LinearSeq[T] = new sc.LinearSeq[T] {
      def apply(i: Int) = xs.apply(i)
      def length = xs.length
      override def head = xs.head
      override def tail = xs.tail.asScalaLinearSeq
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
  //endregion

}
