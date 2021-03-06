package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def iter(syms: Array[Char], index: Int, acc: Int): Boolean =
      if (acc < 0) false
      else if (index > syms.length - 1) acc == 0
      else syms(index) match {
          case '(' => iter(syms, index + 1, acc + 1)
          case ')' => iter(syms, index + 1, acc - 1)
          case _ => iter(syms, index + 1, acc)
        }

    iter(chars, index=0, acc=0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, balance: Int, sign: Int): (Int, Int) =
      if (idx >= until) (balance, sign)
      else {
        val newBalance = chars(idx) match {
          case '(' => balance + 1
          case ')' => balance - 1
          case _ => balance
        }
        val newSign = if (newBalance > 0) 1
        else if (newBalance < 0) -1
        else 0
        traverse(idx + 1, until, newBalance, newSign)
      }

    def reduce(from: Int, until: Int): (Int, Int) =
      if (from <= until || from - until < threshold) traverse(from, until, 0, 0)
      else {
        val middle = (until - from) / 2
        val (left, right) = parallel(reduce(from, middle), reduce(middle + 1, until))
        (left._1 + right._2, if (left._1 == 0) right._2 else left._1 )
      }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
