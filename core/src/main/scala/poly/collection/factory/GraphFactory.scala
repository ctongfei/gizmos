package poly.collection.factory

import poly.collection._
import poly.collection.evidence._

import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait GraphFactory[+G[_, _], Ev[_]] extends Factory2[({type λ[K, E] = (K, K, E)})#λ, G, Ev, NoneEv] {

  def newGraphBuilder[K: Ev, E]: GraphBuilder[K, E, G[K, E]]

  def newBuilder[K: Ev, E: NoneEv]: GraphBuilder[K, E, G[K, E]] = newGraphBuilder

  def fromKeysAndArcs[K: Ev, E](ks: Traversable[K], kkes: Traversable[(K, K, E)]) = {
    val b = newBuilder[K, E]
    b.addKeys(ks)
    b.addArcs(kkes)
    b.result()
  }

  def fromArcs[K: Ev, E](kkes: Traversable[(K, K, E)]) = {
    val b = newBuilder[K, E]
    b.addArcs(kkes)
    b.result()
  }

}
