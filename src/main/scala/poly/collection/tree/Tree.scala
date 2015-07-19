package poly.collection.tree

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Tree[+T] {

  def root: T

  def children: Seq[Tree[T]]

}
