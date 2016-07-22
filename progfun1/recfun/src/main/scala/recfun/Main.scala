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
    var brackets = 0
    var index = 0

    def count(sym: Char) =
      if (sym == '(') brackets = brackets + 1
      else if (sym == ')') brackets = brackets - 1

    def iterate(sym: Char): Boolean = {
      count(sym)
      if (brackets < 0) false
      else if (index != chars.length - 1) {
        index = index + 1
        iterate(chars(index))
      }
      else true
    }

    if (chars.isEmpty) true
    else iterate(chars(index))
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
