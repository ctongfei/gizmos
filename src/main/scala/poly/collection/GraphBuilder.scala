package poly.collection

/**
 * The base trait for graph builders, which are objects that allow
 * incremental construction of graphs.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait GraphBuilder[-K, -E, +G] {

  /**
   * Provides a hint to this builder about how many vertices are expected to be added.
   * @param n The hint how many vertices is to be added
   */
  def numKeysHint(n: Int) = {}

  def numArcsHint(n: Int) = {}

  def addKey(i: K): Unit

  def addArc(i: K, j: K, e: E): Unit

  def addKeys(ks: Traversable[K]) = {
    if (ks.sizeKnown)
      numKeysHint(ks.size)
    ks foreach addKey
  }

  def addArcs(kkes: Traversable[(K, K, E)]) = {
    if (kkes.sizeKnown)
      numArcsHint(kkes.size)
    kkes foreach { case (i, j, e) => addArc(i, j, e) }
  }

  def result(): G

}
