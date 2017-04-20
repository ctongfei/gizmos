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

  def checkTraversable[T](s: sc.Traversable[T], p: pc.Traversable[T]): Boolean = {
    val sa = scm.ArrayBuffer[T]()
    val pa = scm.ArrayBuffer[T]()
    s foreach sa.+=
    p foreach pa.+=
    if (sa.length != pa.length) return false
    var i = 0
    while (i < sa.length) {
      if (sa(i) != pa(i)) return false
      i += 1
    }
    true
  }

  def checkIterable[T](s: sc.Iterable[T], p: pc.Iterable[T]): Boolean = {
    if (checkTraversable(s, p)) {
      val si = s.iterator
      val pi = p.newIterator
      while (si.hasNext && pi.advance())
        if (si.next() != pi.current) return false
      if (si.hasNext || pi.advance()) return false
      true
    }
    else false
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
