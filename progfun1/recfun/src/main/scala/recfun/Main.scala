package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(countChange(5, List(1,2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 1 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def iter(syms: List[Char], acc: Int): Boolean =
      if (acc < 0) false
      else if (syms.isEmpty) acc == 0
      else syms.head match {
        case '(' => iter(syms.tail, acc + 1)
        case ')' => iter(syms.tail, acc - 1)
        case _ => iter(syms.tail, acc)
      }

    iter(chars, acc = 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def rec(m: Int, cs: List[Int], counted: Int): Int =
      if (m < 0) counted
      else if (cs.isEmpty) {
        if (m == 0) counted + 1
        else counted
      }
      else rec(m, cs.tail, counted) + rec(m - cs.head, cs, counted)
    rec(money, coins, 0)
  }
}
