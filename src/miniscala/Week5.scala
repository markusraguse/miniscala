package miniscala

import miniscala.Week2._
import scala.util.Random

object Week5 {

  def merge(xs: IntList,ys: IntList): IntList = (xs,ys) match{
    case (Nil,Nil) => Nil
    case (Cons(x,xss),Nil) => Cons(x,merge(xss,ys))
    case (Nil,Cons(y,yss)) => Cons(y,merge(xs,yss))
    case (Cons(x,xss), Cons(y,yss)) => if(x <= y) Cons(x, merge(xss,ys)) else Cons(y,merge(xs,yss))
  }

  def split(xs: IntList, n: Int): (IntList, IntList) ={
    def split1(xs: IntList, ys: IntList, n: Int): (IntList, IntList) ={
      (xs,n) match{
        case (_,0) =>(ys,xs)
        case (Cons(x,xss),_) => split1(xss,append(ys,x),n-1)
        case (Nil,_) => throw new Error("n is larger than the length of the IntList")
      }
    }
    split1(xs,Nil,n)
  }

   def mergeSort(xs: IntList): IntList = {
    val n = length(xs) / 2
    if (n == 0) xs
    else {
      val (left, right) = split(xs, n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def randomIntList(): IntList = {
    val r = scala.util.Random
    def random1(xs: IntList, n: Int) : IntList = (xs,n) match{
      case (_,0) => xs
      case (_,_) => random1(Cons(r.nextInt(100),xs),n - 1)
    }
    random1(Nil,r.nextInt(100))
  }

  def permuted(xs: IntList, ys: IntList): Boolean = {
    def permuted1(xs: IntList, arr: Array[Int] = new Array[Int](100)): Array[Int] = xs match {
      case Cons(x,xss) => arr(x) += 1; permuted1(xss, arr)
      case Nil => arr
    }
    if(permuted1(xs) sameElements permuted1(ys)) true else false
  }

  def testMergeSort(): Unit = {
    val r = scala.util.Random
    for(_ <- 1 to 10000){
      val xs = randomIntList()
      val ys = mergeSort(xs)
      val zs = split(xs,r.nextInt(length(xs)+1))
      val zss = merge(zs._1,zs._2)

      assert((length(zs._1) + length(zs._2)) == length(xs))

      assert(permuted(zss,xs))

      assert(permuted(xs,ys))

      assert(ordered(ys))
    }
  }


  sealed abstract class IntTree
  case object Leaf extends IntTree
  case class Branch(left: IntTree, x: Int, right: IntTree) extends IntTree

  def size(t: IntTree): Int = {
    t match {
      case Leaf => 0
      case Branch(l,_,r) => 1 + size(l) + size(r)
    }
  }

  def height(t: IntTree): Int = t match {
    case Leaf => 0
    case Branch(left, _, right) => (height(left) max height(right)) + 1
  }

  def flatten(t: IntTree): IntList = {
    t match {
      case Leaf => ???
      case Branch(left, x, right) => ???
    }
  }


  def insert(t: IntTree, x: Int): IntTree = t match {
    case Leaf => Branch(Leaf, x, Leaf)
    case Branch(left, y, right) =>
      if (x <= y) Branch(insert(left, x), y, right)
      else Branch(left, y, insert(right, x))
  }
  def contains(t: IntTree, x: Int): Boolean = {
    t match {
      case Leaf => false
      case Branch(l,y,r) => if(y == x) true else {contains(l,x); contains(r,x)}
    }
  }

  def main(args: Array[String]): Unit = {
    //println(merge(Cons(2, Cons(5, Cons(7, Nil))), Cons(3, Cons(5, Nil))))
    //println(split(Cons(2, Cons(3, Cons(5, Cons(5, Cons(7, Nil))))), 2))
    //println(mergeSort(Cons(9, Cons(8, Cons(2, Nil)))))
    //println(permuted(Cons(2, Cons(5, Cons(7, Nil))),Cons(2, Cons(7, Cons(5, Nil)))))
    testMergeSort()
  }
}