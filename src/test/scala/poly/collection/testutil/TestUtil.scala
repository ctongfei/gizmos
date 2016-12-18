package poly.collection.testutil

import poly.collection.{mut => pcm}
import poly.{collection => pc}

import scala.collection.{mutable => scm}
import scala.{collection => sc}


object TestUtil {

  class SizeMismatchException
    extends Exception

  class ElementMismatchException
    extends Exception

  def checkTraversable[T](s: sc.Traversable[T], p: pc.Traversable[T]) = {
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

  def checkIterable[T](s: sc.Iterable[T], p: pc.Iterable[T]) = {
    checkTraversable(s, p)
    val si = s.iterator
    val pi = p.newIterator
    while (si.hasNext && pi.advance())
      if (si.next() != pi.current) throw new ElementMismatchException
    if (si.hasNext || pi.advance()) throw new SizeMismatchException
  }

  def checkSeq[T](s: sc.Seq[T], p: pc.Seq[T]) = {
    checkIterable(s, p)
    if (s.length != p.length) throw new SizeMismatchException
    for (i ← s.indices) {
      if (s(i) != p(i)) throw new ElementMismatchException
    }
  }

  def checkSet[T](s: sc.Set[T], p: pc.Set[T]) = {
    if (s.size != p.size) throw new SizeMismatchException
    for (x ← s) if (!p.contains(x)) throw new ElementMismatchException
    for (x ← p) if (!s.contains(x)) throw new ElementMismatchException
  }

  def checkMap[K, V](s: sc.Map[K, V], p: pc.Map[K, V]) = {
    if (s.size != p.size) throw new SizeMismatchException
    for (k ← s.keys) if (s(k) != p(k)) throw new ElementMismatchException
    for (k ← p.keys) if (s(k) != p(k)) throw new ElementMismatchException
  }

}
