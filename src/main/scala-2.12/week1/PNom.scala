package week1

/**
  * Created by shexiaogui on 14/04/17.
  */
class PNom {
  private var thredhold: Int = 10;
  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    var sum: Int = 0
    for(index <- s to t) sum = sum + power(a(index), p)
    sum
  }
  
  def sumSegmentPara(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    if((t - s) < thredhold) sumSegment(a, p, s, t)
    val mid = s + (t-s)/2
    val (sum1, sum2) = parallel(sumSegmentPara(a, p, s, mid), sumSegmentPara(a, p, mid, t))
    sum1 + sum2
  }
  
  def power(x: Int, p: Double): Int = math.exp(p * math.log(math.abs(x))).toInt
  
  def parallel[A, B](a: => A, b: =>B): (A, B) = ???
}
