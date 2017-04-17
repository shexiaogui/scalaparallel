package week3

/**
  * Created by shexiaogui on 16/04/17.
  */
trait Iterator[A] {
  def hasNext : Boolean
  def next(): A
}
