package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._

import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait GraphFactory[+G[_, _, _]] {

  implicit def newBuilder[K: Eq, V, E]: GraphBuilder[K, V, E, G[K, V, E]]

  def empty[K: Eq, V, E]: G[K, V, E] = newBuilder[K, V, E].result

  def apply[K: Eq, V, E](vs: (K, V)*)(es: (K, K, E)*): G[K, V, E] = {
    val b = newBuilder[K, V, E]
    b addNodes vs
    b addEdges es
    b.result
  }

  def withoutNodeData[K: Eq, E](es: (K, K, E)*): G[K, Unit, E] = {
    val b = newBuilder[K, Unit, E]
    b addEdges es
    b.result
  }


  //implicit def factory: GraphFactory[G] = this

}

