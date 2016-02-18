package poly.collection.node

import poly.algebra._
import poly.algebra.specgroup._

/**
 * @author Tongfei Chen
 */
trait GraphArc[@sp(i) K, +V, +E] { self =>

  def sourceKey: K = source.key

  def targetKey: K = target.key

  def source: GraphNode[K, V, E]

  def data: E

  def target: GraphNode[K, V, E]

  def equivOnKey: Equiv[K]

  def mapNode[W](f: V => W): GraphArc[K, W, E] = new GraphArc[K, W, E] {
    def source: GraphNode[K, W, E] = self.source.mapNode(f)
    def equivOnKey: Equiv[K] = self.equivOnKey
    def target: GraphNode[K, W, E] = self.target.mapNode(f)
    def data: E = self.data
  }

  def mapArc[F](f: E => F): GraphArc[K, V, F] = new GraphArc[K, V, F] {
    def source: GraphNode[K, V, F] = self.source.mapArc(f)
    def equivOnKey: Equiv[K] = self.equivOnKey
    def target: GraphNode[K, V, F] = self.target.mapArc(f)
    def data: F = f(self.data)
  }

}

object GraphArc {

  def apply[@sp(i) K: Equiv, V, E](s: GraphNode[K, V, E], t: GraphNode[K, V, E], d: E): GraphArc[K, V, E]
    = new GraphArc[K, V, E] {
    def source = s
    def target = t
    def data = d
    def equivOnKey: Equiv[K] = Equiv[K]
  }

}
