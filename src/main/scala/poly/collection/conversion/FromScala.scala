package poly.collection.conversion

import poly.collection._
import poly.collection.node._
import scala.{collection => sc}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 */
object FromScala {

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




}
