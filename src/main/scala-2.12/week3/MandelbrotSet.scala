package week3

/**
  * Created by shexiaogui on 16/04/17.
  */
abstract  class MandelbrotSet {
  private val image: Array[Int] = _
  private val maxIter: Int = _
  def color(i: Int): Int
  private def computePixel(xc: Double, yc: Double, maxIter: Int): Int = {
    var i = 0
    var x, y = 0.0
    while (x * x + y * y < 4 && i < maxIter){
      val xt = x * x - y * y + xc
      val yt = 2 * x * y + yc
      x = xt
      y = yt
      i += 1
    }
    color(i)
  }
  def coordinateFor(index: Int): (Int, Int)
  def parRender(): Unit = {
    for(index <- (0 until image.length).par){
      val (xc, yc) = coordinateFor(index)
      image(index) = computePixel(xc, yc, maxIter)
    }
  }
  def max(xs: Array[Int]): Int = {
    xs.par.fold(Int.MinValue)(math.max)
  }
  def isVowel(char: Char): Boolean
  
  val vowels = (Array('E', 'W', 'u', 'i')).par.aggregate(0)((count, c) => if(isVowel(c)) count + 1 else count, _ + _ )
}
