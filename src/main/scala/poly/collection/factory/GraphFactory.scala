package poly.collection.factory

import poly.collection._
import poly.collection.conversion._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait GraphFactory[+G[_, _, _]] {

  implicit def newBuilder[K, V, E]: GraphBuilder[K, V, E, G[K, V, E]]

  def empty[K, V, E]: G[K, V, E] = newBuilder[K, V, E].result

  def apply[K, V, E](vs: (K, V)*)(es: (K, K, E)*): G[K, V, E] = {
    val b = newBuilder[K, V, E]
    b addNodes vs
    b addEdges es
    b.result
  }


  //implicit def factory: GraphFactory[G] = this

}

