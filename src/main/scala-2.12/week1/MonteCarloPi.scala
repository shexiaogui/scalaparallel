package week1

import scala.util.Random

/**
  * Created by shexiaogui on 14/04/17.
  */
class MonteCarloPi {
  def mcCount(iter: Int): Int = {
    val randomX = new Random()
    val randomY = new Random()
    var sum = 0
    (0 until iter).foreach(_ => {val x = randomX.nextDouble(); val y = randomY.nextDouble(); if((x * x + y * y) < 1) sum += 1})
    sum
  }
  
  def monteCarloPiSeq(iter: Int): Double = {
    4.0 * mcCount(iter) / iter
  }
  
  def monteCarloPiParallel(iter: Int): Double = {
  val ((para1, para2), (para3, para4)) =  parallel(parallel(mcCount(iter/4), mcCount(iter/4)), parallel(mcCount(iter/4), mcCount(iter - 3 * iter/4)))
    (para1 + para2 + para3 + para4) * 4.0 / iter
  }
  
  def parallel[A, B](a: => A, b: =>B): (A, B) = ???
  
}
