package week2

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
/**
  * Created by shexiaogui on 15/04/17.
  */
@RunWith(classOf[JUnitRunner])
class MergeSortTest extends FunSuite{
  test("quick sort test"){
    val array = Array(2, 33, 4, 5, 45, 22, 86, 1, 344)
    val expectedArray = Array(1, 2, 4, 5, 22, 33, 45, 86, 344)
    val mergeSort = new MergeSort
    mergeSort.quickSort(array, 0, array.length - 1)
    assert(array.zip(expectedArray).forall(pair => pair._2 == pair._1))
  }
  
  
  test("merge from src to dest"){
    val src = Array(1, 3, 5, 7, 9, 2, 4, 6, 8, 10)
    val dest = new Array[Int](src.length)
    val expectedArray = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val mergeSort = new MergeSort
    mergeSort.merge(src, dest, 0, src.length / 2 , src.length)
    assert(dest.zip(expectedArray).forall(pair => pair._2 == pair._1))
  }
  
  
  test("merge from src to dest 2 "){
    val src = Array(1, 3, 5, 2, 4, 6, 8, 10)
    val dest = new Array[Int](src.length)
    val mergeSort = new MergeSort
    val expectedArray = Array(1, 2, 3, 4, 5, 6, 8, 10)
    mergeSort.merge(src, dest, 0, 3, src.length)
    assert(dest.zip(expectedArray).forall(pair => pair._2 == pair._1))
  }
  
  test("merge from src to dest 3 "){
    val src = Array(1, 3, 5, 7, 9, 2, 4)
    val dest = new Array[Int](src.length)
    val expectedArray = Array(1, 2, 3, 4, 5, 7, 9)
    val mergeSort = new MergeSort
    mergeSort.merge(src, dest, 0, 5, src.length)
    assert(dest.zip(expectedArray).forall(pair => pair._2 == pair._1))
  }
  
  test("mergeSortPara"){
    val array = Array(2, 33, 4, 5, 45, 22, 86, 1, 344)
    val expectedArray = Array(1, 2, 4, 5, 22, 33, 45, 86, 344)
    val mergeSort = new MergeSort
    mergeSort.parMergeSort(array, 5)
    println(array.mkString(" "))
//    assert(array.zip(expectedArray).forall(pair => pair._2 == pair._1))
  }
}
