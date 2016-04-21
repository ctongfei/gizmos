import poly.collection.mut._
import poly.algebra.syntax._

val ft = FenwickTree(1, 2, 3, 4, 5, 6, 7)
ft.cumulativeSum(5)
