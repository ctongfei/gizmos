package poly.collection

import scala.{collection => sc}
import scala.collection.{mutable => scm}
import poly.{collection => pc}
import poly.collection.{mut => pcm}

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object TestUtil {

  def checkTraversable[T](s: sc.Traversable[T], p: pc.Traversable[T]) = {
    val sa = scm.ArrayBuffer[T]()
    val pa = scm.ArrayBuffer[T]()
    s foreach sa.+=
    p foreach pa.+=
    sa equals pa
  }

  def checkIterable[T](s: sc.Iterable[T], p: pc.Iterable[T]): Boolean = {
    val si = s.iterator
    val pi = p.newIterator
    while (si.hasNext && pi.advance())
      if (si.next() != pi.current) return false
    if (si.hasNext || pi.advance()) return false
    true
  }

  def checkSeq[T](s: sc.Seq[T], p: pc.Seq[T]): Boolean = {
    if (s.length != p.length) return false
    for (i ← s.indices) {
      if (s(i) != p(i)) return false
    }
    true
  }

  def checkSet[T](s: sc.Set[T], p: pc.Set[T]): Boolean = {
    if (s.size != p.size) return false
    for (x ← s) if (!p.contains(x)) return false
    for (x ← p) if (!s.contains(x)) return false
    true
  }

  def checkMap[K, V](s: sc.Map[K, V], p: pc.Map[K, V]): Boolean = {
    if (s.size != p.size) return false
    for (k ← s.keys) if (s(k) != p(k)) return false
    for (k ← p.keys) if (s(k) != p(k)) return false
    true
  }

}
