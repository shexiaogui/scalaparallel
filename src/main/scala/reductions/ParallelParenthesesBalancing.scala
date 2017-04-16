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
    chars(0) = ')'
    chars(length - 1) = '('
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
    var opening = 0
    var i = 0
    while(i < chars.length){
      if(chars(i) == '(') opening += 1
      else if(chars(i) == ')' && opening == 0) return false
      else if(chars(i) == ')' && opening != 0)  opening -= 1
      i += 1
    }
    opening == 0
  }
  

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, opening: Int, closing: Int):(Int, Int)= {
      var i = from
      var left = 0
      var right = 0
      while(i < until){
        if(chars(i) == '(') left += 1
        else if (chars(i) == ')') right += 1
        i += 1
      }
      ((opening - right), (closing - left))
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val ((opening1, closing1), (opening2, closing2)) = parallel(reduce(from, mid), reduce(mid, until))
        var closeRes = 0
        var openRes = 0
        if(opening1 < closing2) closeRes += (closing2 - opening1)
        else if(opening1 > closing2) openRes += (opening1 - closing2)
        else if(opening2 > closing1) openRes += (opening2 - closing1)
        else if(opening2 < closing1) closeRes += (closing1 - opening2)
        (openRes, closeRes)
      }
    
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
