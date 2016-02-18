package poly.collection.node

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait GraphNodeLike[K, +V, +E, +N <: GraphNodeLike[K, V, E, N]]
  extends ForwardNodeLike[V, GraphNodeLike[K, V, E, N]] with KeyedNodeLike[K, V, GraphNodeLike[K, V, E, N]] { self: N =>

  def outgoingKeySet: Set[K]

  def outgoingKeys = outgoingKeySet.elements

  def outgoingNodes: Iterable[N]

  def outgoingArcs: Iterable[GraphArc[K, V, E]]

  def outDegree = outgoingKeys.size

  def succ = outgoingNodes

}

trait GraphNode[K, +V, +E] extends GraphNodeLike[K, V, E, GraphNode[K, V, E]] { self =>

  def mapNode[W](f: V => W): GraphNode[K, W, E] = new GraphNode[K, W, E] {
    def outgoingKeySet = self.outgoingKeySet
    def outgoingNodes = self.outgoingNodes map (_ mapNode f)
    def outgoingArcs = self.outgoingArcs map (_ mapNode f)
    def key = self.key
    def data = f(self.data)
    def isDummy = self.isDummy
  }

  def mapArc[F](f: E => F): GraphNode[K, V, F] = new GraphNode[K, V, F] {
    def outgoingKeySet = self.outgoingKeySet
    def outgoingArcs = self.outgoingArcs map (_ mapArc f)
    def outgoingNodes = self.outgoingNodes map (_ mapArc f)
    def key = self.key
    def data = self.data
    def isDummy = self.isDummy
  }

}
