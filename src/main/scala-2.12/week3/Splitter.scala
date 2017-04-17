package week3

import java.util.concurrent.ForkJoinTask
import common._
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

trait Combiner[A, Repr]{
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

