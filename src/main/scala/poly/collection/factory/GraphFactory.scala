package poly.collection.factory

import poly.collection._
import poly.collection.conversion._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait GraphFactory[+G[_, _, _]] {

  implicit def newBuilder[I, V, E]: GraphBuilder[I, V, E, G[I, V, E]]

  def empty[I, V, E]: G[I, V, E] = newBuilder[I, V, E].result

  def apply[I, V, E](vs: (I, V)*)(es: (I, I, E)*): G[I, V, E] = {
    val b = newBuilder[I, V, E]
    b addVertices vs
    b addEdges es
    b.result
  }

  implicit def factory: GraphFactory[G] = this

}

