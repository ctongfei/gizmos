import poly.collection.mut._

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

val q = ArrayQueue[Int]()
q.enqueue(0)
q
q.enqueue(1)
q
