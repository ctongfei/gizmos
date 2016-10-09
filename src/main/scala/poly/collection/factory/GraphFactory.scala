package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait GraphFactory[+G[_, _]] {

  implicit def newBuilder[K: Eq, E]: GraphBuilder[K, E, G[K, E]]

  def fromEdges[K: Eq, E](kkes: Traversable[(K, K, E)]) = {
    val b = newBuilder[K, E]
    b.addEdges(kkes)
    b.result
  }

  def apply[K: Eq, E](kkes: (K, K, E)*) = fromEdges(kkes)

}
