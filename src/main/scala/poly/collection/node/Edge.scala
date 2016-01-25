package poly.collection.node

/**
 * @author Tongfei Chen
 */
trait Edge[+E] {

  def data: E
  def source: Node[_]
  def target: Node[_]

}
