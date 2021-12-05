class B(val X:Int){
  override def toString() =
    s"b: $X"
}

class A {
  val b = List(new B(1), new B(2), new B(2))
}


val L = List(new A)

println(L)

val res = L.filter(x => x.b.exists(y => y.X == 2)).map(a => a.b)

println(res)

L.flatMap(_.b.filter(_.X == 2))
