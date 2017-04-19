package week3

import java.util.concurrent.ForkJoinTask

import common._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
/**
  * Created by shexiaogui on 16/04/17.
  */
trait Splitter[A] extends Iterator[A]{
  def split: Seq[Splitter[A]]
  def remaining: Int // num of remaning
  private val threshold = _
  
  def foldLeft(z: A)(f: (A, A) => A)
  
  def fold(z: A)(f: (A, A) => A) = {
    if(remaining < threshold) foldLeft(z)(f)
    else{
      val children: Seq[ForkJoinTask[A]] = for(child <- split) yield task{child.fold(z)(f)}
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}

trait Builder[A, Repr] {
  def +=(elem: A): Builder[A, Repr]
  def result: Repr
  def newBuilder: Builder[A, Repr]
}

trait Combiner[A, Repr] {
  def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
  def newCombiner: Combiner[A, Repr]
}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def newCombiner: Combiner[T, Traversable[T]]
  
  def filter(p: T => Boolean): Traversable[T] = {
    val builder = newBuilder
    foreach( x => if(p(x)) builder += x ) // impl in parallel using splitter and combiner
    builder.result
  }
  
}

class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int) {
  private var numElem = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]
  buffers += new ArrayBuffer[T]()
  
  def +=(x: T) = {
    buffers.last += x
    numElem += 1
    this
  }
  
  def combine[N <: T, That >: Array[T]](that: Combiner[N, That]) = {
    (that: @unchecked) match {
      case that: ArrayCombiner[T] =>
        buffers ++= that.buffers
        numElem += that.numElem
        this
    }
    
  }
  
  def size = numElem
  
  def copyTo(array: Array[T], from: Int, end: Int): Unit = {
    var i = from
    var j = 0
    while(i >= buffers(j).length){ // move i j to position from
      i -= buffers(j).length
      j +=1
    }
    
    var k = from // start copy from "from"
    while(k < end){ // until end
      array(k) = buffers(j)(i)
      i += 1
      if(i >= buffers(j).length){ // if the next position i gets into next buffer, move to next buffer
        i = 0
        j += 1
      }
      k += 1
    }
     
  }
  def result: Array[T] = {
    val array = new Array[T](numElem)
    val step = math.max(1, numElem/parallelism)
    val starts = (0 until numElem) by step
    val chunks = starts.zip(starts.tail)
    val tasks = for((from, end) <- chunks) yield task{
      copyTo(array, from, end)
    }
    tasks.foreach(_.join())
    array
  }
}

