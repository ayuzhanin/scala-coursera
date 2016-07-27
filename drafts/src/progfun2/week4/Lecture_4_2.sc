

/**
  * Week 4: Timely Effects
  * Lecture 4.2 - Functional Reactive Programming
  */
//
//class BankAccount {
//  val balance = Var(0)
//
//  def deposit(amount: Int): Unit =
//    if (amount > 0) {
//      val current = balance()
//      balance() = current + amount
//    }
//
//  def withdraw(amount: Int): Unit =
//    if (0 < amount && amount <= balance) {
//      val current = balance()
//      balance() = current - amount
//    }
//    else throw new Error("insufficient funds")
//}
//
//object Consolidator{
//  def consolidated(observed: List[BankAccount]): Signal[Int] =
//    Signal(observed map (_.balance()) sum )
//}
//
///**
//  * Usage
//  */
//
//var one = new BankAccount
//var two = new BankAccount
//var consolodator = consolodated(List(one, two))
//
//one deposit 50
//consolodator()
//
//two deposit 30
//consolodator()