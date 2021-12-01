class B(val X:Int){
  override def toString =
    s"b:$X"
}

class A(val b: List[B]) {
  override def toString =
    s"a: $b"
}

val listA1 = List(new B(1), new B(2), new B(3))
val listA2 = List(new B(1), new B(7), new B(12))
val listA3 = List(new B(9), new B(5), new B(3))

val L = List(new A(listA1), new A(listA2), new A(listA3))

println(L)

val res = L.filter(x => x.b.exists(y => y.X == 3)).map(a => a.b)

println(res)
