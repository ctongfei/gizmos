package poly.collection.graph

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

trait IntIndexedGraphFactory[+G[_, _]] {

  implicit def newBuilder[V, E]: GraphBuilder[Int, V, E, G[V, E]]

  def empty[V, E]: G[V, E] = newBuilder[V, E].result

  def apply[V, E](vs: V*)(es: (Int, Int, E)*): G[V, E] = {
    val b = newBuilder[V, E]
    b addVertices vs.zipWithIndex.map(_.swap) //TODO: efficiency?
    b addEdges es
    b.result
  }

  implicit def factory: IntIndexedGraphFactory[G] = this

}