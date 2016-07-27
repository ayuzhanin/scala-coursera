/**
  * Week 4: Timely Effects
  * Lecture 4.1 - Imperative Event Handling: The Observer Pattern
  */

trait Subscriber {
  def handler(publisher: Publisher)
}

trait Publisher {
  private var subs: Set[Subscriber] = Set()

  def subscribe(sub: Subscriber): Unit =
    subs += sub

  def unsubscribe(sub: Subscriber): Unit =
    subs -= sub

  def publish(): Unit =
    subs foreach (_.handler(this))
}

/**
  * Let's implement Publisher
  */

class BankAccount extends Publisher {
  private var _balance = 0

  def balance_=(amount: Int) =
    if (amount > 0) {
      _balance = amount
      publish()
    }

  def balance = _balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount
      publish()
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= _balance) {
      balance = balance - amount
      publish()
    }
    else throw new Error("insufficient funds")
}

/**
  * Let's implement Subscriber
  */

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))
  compute()

  private var _totalBalance: Int = _

  def totalBalance = _totalBalance

  private def totalBalance_=(balance: Int): Unit  =
  _totalBalance = balance


  private def compute(): Unit =
    totalBalance = observed map (_.balance) sum

  override def handler(pub: Publisher): Unit = compute()
}


/**
  * Try examples
  */

var one = new BankAccount
var two = new BankAccount
var three = new BankAccount
var four = new BankAccount

var consolidator = new Consolidator(List(one, two, three, four))
consolidator.totalBalance

one.balance = 40
consolidator.totalBalance

two deposit 50
consolidator.totalBalance

three deposit 40
consolidator.totalBalance

two withdraw 40
consolidator.totalBalance