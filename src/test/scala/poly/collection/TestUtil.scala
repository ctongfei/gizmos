package poly.collection

import scala.{collection => sc}
import scala.collection.{mutable => scm}
import poly.{collection => pc}
import poly.collection.{mut => pcm}

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object TestUtil {

  class SizeMismatchException
    extends Exception

  class ElementMismatchException
    extends Exception

  implicit class checkTraversable[T](s: sc.Traversable[T]) {
    def ==?==(p: pc.Traversable[T]) = {
      val sa = scm.ArrayBuffer[T]()
      val pa = scm.ArrayBuffer[T]()
      s foreach sa.+=
      p foreach pa.+=
      if (sa.length != pa.length) throw new SizeMismatchException
      var i = 0
      while (i < sa.length) {
        if (sa(i) != pa(i)) throw new ElementMismatchException
        i += 1
      }
    }
  }

  implicit class checkIterable[T](s: sc.Iterable[T]) {
    def ==?==(p: pc.Iterable[T]) = {
      val si = s.iterator
      val pi = p.newIterator
      while (si.hasNext && pi.advance())
        if (si.next() != pi.current) throw new ElementMismatchException
      if (si.hasNext || pi.advance()) throw new SizeMismatchException
    }
  }

  implicit class checkSeq[T](s: sc.Seq[T]) {
    def ==?==(p: pc.Seq[T]) = {
      if (s.length != p.length) throw new SizeMismatchException
      for (i ← s.indices) {
        if (s(i) != p(i)) throw new ElementMismatchException
      }
    }
  }

  implicit class checkSet[T](s: sc.Set[T]) {
    def ==?==(p: pc.Set[T]) = {
      if (s.size != p.size) throw new SizeMismatchException
      for (x ← s) if (!p.contains(x)) throw new ElementMismatchException
      for (x ← p) if (!s.contains(x)) throw new ElementMismatchException
    }
  }

  implicit class checkMap[K, V](s: sc.Map[K, V]) {
    def ==?==(p: pc.Map[K, V]) = {
      if (s.size != p.size) throw new SizeMismatchException
      for (k ← s.keys) if (s(k) != p(k)) throw new ElementMismatchException
      for (k ← p.keys) if (s(k) != p(k)) throw new ElementMismatchException
      true
    }
  }

}
