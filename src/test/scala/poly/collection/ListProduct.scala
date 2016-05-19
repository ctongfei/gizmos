package poly.collection

import poly.collection.mut._
import poly.collection.conversion.FromScala._
import scala.util.control.Breaks._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object ListProduct extends App {

  def listProduct[T](ls: Seq[Seq[T]]): Iterable[Seq[T]] = Iterable.ofIterator {
    new Iterator[Seq[T]] {
      val n = ls.length
      var is = ls.map(_.newIterator).to(ArraySeq)
      var first = true
      def current = is.map(_.current)
      def advance() = {
        if (first) {
          first = false
          is forall (_.advance())
        } else {
          var advanced = false
          breakable {
            for (i ← Range(n)) {
              if (is(i).advance()) {
                advanced = true; break
              }
              is(i) = ls(i).newIterator
              is(i).advance()
            }
          }
          advanced
        }
      }
    }
  }


  val a = ArraySeq(1, 2, 3)
  val b = ArraySeq(4, 5)
  val c = ArraySeq(6, 7, 8)

  val abc = ArraySeq(a, b, c)
  val di = ArraySeq.from(abc.map(_.newIterator))
  di foreach (i => i.advance())

  val d = listProduct(ArraySeq(a, b, c))

  d foreach println

  val bp = 0

}
