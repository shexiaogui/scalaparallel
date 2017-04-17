package week3

import java.util.concurrent.ConcurrentSkipListSet

import scala.collection.GenSet
import scala.collection.concurrent.TrieMap

/**
  * Created by shexiaogui on 16/04/17.
  */
class IntersectionSet {
  def intersection(a: GenSet[Int], b: GenSet[Int]) = {
    val result = new ConcurrentSkipListSet[Int]()
    for(x <- a) if (b.contains(x)) result.add(x)
    result
  }
  def TrieMapCurrent = {
    val graph = new TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
    graph(graph.size - 1) = 0
    val previous = graph.snapshot()
    for((k, v) <- graph.par) graph(k) = previous(v)
  }
}
