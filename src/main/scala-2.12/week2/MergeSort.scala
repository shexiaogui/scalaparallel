package week2

import common._
/**
  * Created by shexiaogui on 15/04/17.
  */
class MergeSort {
  
  
  def parMergeSort(xs: Array[Int], maxDepth: Int): Unit = {
    val ys = new Array[Int](xs.length)
    
    def sort(from: Int, to: Int, depth: Int): Unit = {
      if (depth == maxDepth) quickSort(xs, from, to - 1)
      else {
        val mid = (from + to) / 2
        val right= task{
         sort(mid, to, depth + 1)
        }
        sort(from, mid, depth + 1)
        right.join()
        
        val flip = (maxDepth - depth) % 2 == 0
        val src = if (flip) ys else xs
        val dest = if (flip) xs else ys
        merge(src, dest, from, mid, to)
      }
    }
  
  
    def copy(src: Array[Int], target: Array[Int], from: Int, to: Int, depth: Int): Unit = {
      if (depth == maxDepth) Array.copy(src, from, target, from, to - from)
      else {
        val mid = (to + from) / 2
        val left = task{copy(src, target, from, mid, depth + 1)}
        copy(src, target, mid, to, depth + 1)
        left.join()
      }
    }
  
    sort(0, xs.length, 0)
    if(maxDepth % 2 == 0){println(ys.mkString(" ")); copy(ys, xs, 0, xs.length, 0)}
  
  }
  
  def quickSort(array: Array[Int], start: Int, end: Int): Unit = {
    if (start >= end) None
    else{
      val pivot = array(start + (end - start) / 2)
      var i = start
      var j = end
      while (i <= j) {
        while (i <= j && array(i) < pivot) i += 1
        while (i <= j && array(j) > pivot) j -= 1
        if (i <= j) {
          val tmp = array(i)
          array(i) = array(j)
          array(j) = tmp
          i += 1
          j -= 1
        }
      }
      quickSort(array, start, j)
      quickSort(array, i, end)
    }
   
  }
  
  def merge(src: Array[Int], dest: Array[Int], from: Int, mid: Int, to: Int): Unit = {
    var i = from
    var j = mid
    var next = from
    while(i < mid && j < to){
      if(src(i) < src(j)){dest(next) = src(i); next += 1; i += 1}
      else{dest(next) = src(j); next += 1; j += 1}
    }
    if(i == mid) Array.copy(src, j , dest, next, to - j)
    if(j == to) Array.copy(src, i, dest, next , mid - i)
  }
  
  
}