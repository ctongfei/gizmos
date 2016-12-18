package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait GraphFactory[+G[_, _]] {

  implicit def newBuilder[K: Eq, E]: GraphBuilder[K, E, G[K, E]]

  def fromKeysAndArcs[K: Eq, E](ks: Traversable[K])(kkes: Traversable[(K, K, E)]) = {
    val b = newBuilder[K, E]
    b.addKeys(ks)
    b.addArcs(kkes)
    b.result()
  }

  def fromArcs[K: Eq, E](kkes: Traversable[(K, K, E)]) = {
    val b = newBuilder[K, E]
    b.addArcs(kkes)
    b.result()
  }

  def apply[K: Eq, E](ks: K*)(kkes: (K, K, E)*) = fromKeysAndArcs(ks)(kkes)

  def apply[K: Eq, E](kkes: (K, K, E)*) = fromArcs(kkes)

}
