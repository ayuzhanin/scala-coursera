package progfun2.week4

/**
  * Simple Functional Reactive Programming Implementation
  */

class Signal[T](expr: => T) {

  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()

  private val caller = new StackableVariable[Signal[_]](NoSignal)

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "Cyclic singnal definition")
    myValue
  }

  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

}

object Signal {
  //private val caller = new StackableVariable[Signal[_]](NoSignal)
  def apply[T](expression: => T) = new Signal(expression)
}

object NoSignal extends Signal[Nothing](???)

class Var[T](expr: => T) extends Signal[T](expr){
  override def update(expr: => T): Unit = ???
}

object Var{
  def update[T](expr: => T): Unit = new Var(expr)
}

class StackableVariable[T](init: T){
  private var values: List[T] = List(init)
  def value: T = values.head
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op
    finally values = values.tail
  }
}