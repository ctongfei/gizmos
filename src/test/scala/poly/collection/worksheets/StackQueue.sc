import poly.collection.mut._
import poly.algebra.syntax._

val s = ArrayStack[Int]()
s.push(0)
s
s.push(1)
s
s.pushAll(ArraySeq(2, 3, 4))
s

s.pop()
s.top
s.pop()
s.top

s.size

val q = BinaryHeap[Int]()
q += 0
q += 1
q += 2
q += 3
q += 4
q += 5
q
q.pop()
q
q.pop()
q
q.pop()
q
q += 6
q += 7
q += 8
q += 9
q
