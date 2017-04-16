package week2

import common._
/**
  * Created by shexiaogui on 15/04/17.
  */





abstract class ParallelScan {
  def reduceSegment[A](input: Array[A], left: Int, right: Int, a0: A, f: (A, A)=> A): A
  def mapSegment[A, B](input: Array[A], left: Int, right: Int, fi: (Int, A) => B, out: Array[B]): Unit
  
  def scanLeft[A](input: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {// something like fibonacci
    val fi = {(i: Int, v: A) => reduceSegment(input, 0, i, a0, f)}
    mapSegment(input, 0, input.length, fi, out)
    val last = input.length - 1
    out(last + 1) = f(out(last), input(last))
  }
  
  
  sealed abstract class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  
  
  sealed abstract class TreeRes[A] {val res: A }
  case class LeafRes[A](override val res: A) extends TreeRes[A]
  case class NodeRes[A](left: TreeRes[A], override val res: A, right: TreeRes[A]) extends TreeRes[A]
  
  def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
    case Leaf(v) => LeafRes(v)
    case Node(left, right) =>{
      val (tLeft, tRight) = (reduceRes(left, f), reduceRes(right, f))
      NodeRes(tLeft, f(tLeft.res, tRight.res), tRight)
    }
  }
  
  def upSweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match { // from leaf to root of the tree. which is parallel version of reduceRes
    case Leaf(v) => LeafRes(v)
    case Node(left, right) =>{
      val (tLeft, tRight) = parallel(upSweep(left, f), upSweep(right, f))
      NodeRes(tLeft, f(tLeft.res, tRight.res), tRight)
    }
  }
  
  def downSweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
    case LeafRes(a) => Leaf(f(a0, a))
    case NodeRes(left, _, right) => {
      val (tLeft, tRight) = parallel(downSweep[A](left, a0, f), downSweep(right, f(a0, left.res), f)) // a0 reduce of all element left of tree t
      Node(tLeft, tRight)
    }
  }
  
  def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
    case Leaf(v) => Node(Leaf(x), Leaf(v))
    case Node(left, right) => Node(prepend(x, left), right)
  }
  
  def scanLeftWithTree[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
    val tRes = upSweep(t, f)
    val scanned = downSweep(tRes, a0, f)
    prepend(a0, scanned)
  }
  
  private val threshold: Int = _
  sealed abstract class TreeResRange[A]{val res: A}
  case class LeafResRange[A](from: Int, to: Int, override val res: A) extends TreeResRange[A]
  case class NodeResRange[A](left: TreeResRange[A], override val res: A, right: TreeResRange[A]) extends TreeResRange[A]
 
  def reduceSeq[A](input: Array[A], from: Int, to: Int, a0: A, f: (A, A) => A): A = {
    var a = a0
    var i = from
    while(i < to){
      a = f(a, input(i))
      i += 1
    }
    a
  }
  
  def scanLeftSeg[A](input: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A, output: Array[A]) = {
    var a = a0
    var i = left
    output(0) = a0
    while(i < right){
      a = f(a, input(i))
      i += 1
      output(i) = a
    }
  }
  def upSweepRnage[A](input: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResRange[A] = {
    if(to - from < threshold) LeafResRange(from, to, reduceSeq(input, from + 1, to, input(from), f))
    else{
      val mid = from + (to - from) / 2
      val (tLeft, tRight) = parallel(upSweepRnage(input, from, mid, f), upSweepRnage(input, mid, to, f))
      NodeResRange(tLeft, f(tLeft.res , tRight.res), tRight)
    }
  }
  
  def downSweepRange[A](input: Array[A], a0: A, f:(A, A) => A, t: TreeResRange[A], out: Array[A]): Unit = t match {
    case LeafResRange(from, to, res) => scanLeftSeg(input, from, to, a0, f, out)
    case NodeResRange(left, _, right) => {
      parallel(downSweepRange(input, a0, f, left, out), downSweepRange(input, f(a0, left.res), f, right,out))
    }
  }
  
}

