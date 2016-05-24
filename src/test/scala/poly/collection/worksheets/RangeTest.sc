import poly.collection._
import poly.algebra.syntax._

Range(0, 10).sum


0 ~<~ 3
3 ~>~ 0
0 ~~< 4
4 >~~ 0
0 <~~ 4
4 ~~> 0

val a = new Range.Ascending(1, 22, 4)
val b = new Range.Ascending(2, 15, 1)
a intersect b


Range(0, 10)
Range(3, 12)
Range(0, 10, 3)
Range(0, 9, 3)
Range(10, 0, -1)
Range(10, 1, -3)
Range(10, 0, -3)
Range(0, 9, 3).reverse
Range(0, 10, 3).reverse
Range(10, 1, -3).reverse
Range(10, 0, -3).reverse

Range.inclusive(10)
Range.inclusive(2, 10)
Range.inclusive(0, 10, 3)
Range.inclusive(0, 9, 3)
Range.inclusive(10, 0, -3)
Range.inclusive(10, 1, -3)
Range.inclusive(10, 1, -3).reverse
Range.inclusive(10, 0, -3).reverse
Range.inclusive(0, 9, 2).reverse


