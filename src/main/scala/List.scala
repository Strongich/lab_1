package com.tkroman.kpi.y2022.l1
import scala.collection.mutable
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec


enum List[+A]:
  case Nil
  case Cons(h: A, tl: List[A])

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    def go(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(xh, xt) => go(xt, f(acc, xh))
    }
    go(xs, z)
  }
  
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    this match
      case Nil => z
      case Cons(xh, xt) => f(xh, xt.foldRight(z)(f))
  }
  
  def foldRight2[A,B](xs: List[A],z: B)(f: (A, B) => B): B = xs.foldLeft(xs,z)( (a,b)=>f(b,a) )
  
  def concat[A](xs: List[A], ys: List[A]): List[A] = {
    @scala.annotation.tailrec
    def go(xs: List[A],acc: List[A]): List[A] = {
      xs match
        case Nil => acc
        case Cons(xh,xt) => go(xt, Cons(xh, acc))
    }
    go(xs.reverse, ys)
  }

  def flatMap[B](f: A => List[B]): List[B] = {
    this match {
      case Nil => Nil
      case Cons(xh: B, xt: List[A]) => concat(f(xh), xt.flatMap(f))
    }
    this match {
      case Nil => Nil
      case Cons(xh: A, xt) =>
        val acc: List[B] = Nil
        foldRight(acc) { (a, temp) => concat(f(a),temp) }
    }
  }
  
  def zip[B](ys: List[B]):List[(A,B)] = {
    def go(xs: List[A], ys: List[B], acc: List[(A, B)]): List[(A, B)] = (xs, ys) match {
      case (Nil, _) | (_, Nil) => acc.reverse
      case (Cons(xh, xt: List[A]), Cons(yh, yt: List[A])) =>
        val d = (xh, yh)
        go(xt, yt, Cons(d, acc))
    }

    go(this, ys, Nil)
  }

  def partition(pred: A => Boolean): (List[A],List[A]) = {
    def go(xs: List[A])(left: List[A], right: List[A]): (List[A], List[A]) = xs match {
      case Nil => (left.reverse, right.reverse)
      case Cons(xh, xt) =>  if pred(xh) then go(xt)(Cons(xh, left), right)
      else go(xt)(left, Cons(xh, right))
 }
    go(this)(Nil,Nil)
  }

  def reverse: List[A] = {
    def go(xs: List[A],acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(xh, xt) => go(xt, Cons(xh,acc))
    }
    go(this,Nil)
  }


  override def toString: String = {
    def go(sb: StringBuilder, xs: List[A]): String =
      {
        xs match
          case Nil => sb.append("]").result
          case Cons(xh,xt) => go(sb.append(xh).append(if xt == Nil then "" else ", "),xt)
      }
    go(new StringBuilder("["),this)
}
  def range(n: Int): List[Int] = {
    @scala.annotation.tailrec
    def go(n: Int, acc: List[Int] = Nil): List[Int] = {
      n match
        case 0 => acc
        case _ => go(n - 1, Cons(n, acc))
    }
    go(n)
  }

import List.*
object List:
  def empty[A]: List[A] = Nil
  def of[A](xs: A*): List[A] = xs.foldRight(Nil: List[A])(Cons(_, _))

@main def run = {
  println("Hello")
  val a = Seq.range(1,100000).foldRight(0)(_-_)
  val b = List.of().range(100000).foldRight2(List.of().range(100000),0)(_-_)
  println(a)
  println(b)
}
