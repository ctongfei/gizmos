package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object +: {
  def unapply[T](t: Seq[T]) = {
    if (t.isEmpty) None
    else Some((t.head, t.tail))
  }
}

object :+ {
  def unapply[T](t: Seq[T]) = {
    if (t.isEmpty) None
    else Some((t.init, t.last))
  }

}

object :/ {
  private[poly] class RootWithRightChild[+T](val root: T, val right: BinaryTree[T])
  def unapply[T](t: BinaryTree[T]) = {
      if (t.isEmpty) None
      else Some((t.left, new RootWithRightChild[T](t.root, t.right)))
  }
}

object \: {
  def unapply[T](t: :/.RootWithRightChild[T]) = {
    Some(t.root, t.right)
  }
}