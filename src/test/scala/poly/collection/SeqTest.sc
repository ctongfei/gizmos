import poly.collection._
import poly.collection.mut._

val a = ListSeq(0, 1, 2, 0, 3, 4).asSeq
val b = ArraySeq(5, 6, 7, 8).asSeq

a.take(3)
a.takeWhile(_ <= 3)
a.takeTo(_ >= 3)
a.takeUntil(_ > 3)
a.take(10)

a.skip(2)
a.skip(10)
a.skipWhile(_ > 100)
