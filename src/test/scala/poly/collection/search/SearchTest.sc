
import poly.collection.conversion.Scala._
import poly.collection.search._

val g = Map(1 → List(1, 2, 3), 2 → List(4, 5), 3 → List(6), 4 → Nil, 5 → Nil, 6 → List(7), 7 → Nil)

implicit val ss = new StateSpace[Int] {
  def succ(x: Int) = g.
}

