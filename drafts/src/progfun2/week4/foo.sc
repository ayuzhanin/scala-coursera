def sum(a: Int, b: Int) = {
  println("oops, side effect")
  a + b
}

def printTwiceByValue(value: Int) = {
  println(value)
  println(value)
}

def printTwiceByName(value: => Int) = {
  println(value)
  println(value)
}

printTwiceByValue(sum(1,2))

printTwiceByName(sum(1,2))


class Signal[T](expr: => T) {

  var anotherExpr = expr

}

val signal = new Signal(sum(1,2))
val signal2 = new Signal(sum(1,2))

println(signal.anotherExpr.getClass)
println(signal2.anotherExpr.getClass.getTypeName)

val fun = () => Int

fun.getClass

val funfun: Function0[Int] = () => 4
funfun.getClass.getTypeName
fun.getClass.getTypeName

funfun()

().getClass.getCanonicalName

val r = new Unit()
r.getClass.getName
r.getClass.getCanonicalName
r.getClass.getSimpleName
r.getClass.getTypeName