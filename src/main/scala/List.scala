package com.tkroman.kpi.y2022.l1
import scala.collection.mutable

// (0)
enum List[+A]:
  // (1)
  case Nil extends List[Nothing]
  case Cons(h: A, tl: List[A]) extends List[A]

  // (2)
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    this match
      case Nil => z
      case Cons(xh, xt) => f(xh, xt.foldRight(z)(f))
  }

  // (3)
  def concat[A](xs: List[A], ys: List[A]): List[A] = {
    @scala.annotation.tailrec
    def go[A](xs: List[A], acc: List[A]): List[A] = {
      xs match
        case Nil => acc
        case Cons(xh: A, xt: List[A]) => go(xt, Cons(xh, acc))
    }
    go(xs.reverse(xs), ys)
  }

  // (4)
  def flatMap[B](f: A => List[B]): List[B] = {
    this match {
      case Nil => Nil
      case Cons(xh: B, xt: List[A]) => concat(f(xh), xt.flatMap(f))
    }
    // (5)
    this match {
      case Nil => Nil
      case Cons(xh: A, xt) =>
        val temp: List[B] = Nil
        foldRight(temp) { (a, temp) => concat(f(a), temp) }
    }
  }
  // (6)
  def zip[B](ys: List[B]):List[(A,B)] = {
    def go(xs: List[A], ys: List[B], acc: List[(A, B)]): List[(A, B)] = (xs, ys) match {
      case (Nil, _) | (_, Nil) => acc.reverse(acc)
      case (Cons(xh, xt: List[A]), Cons(yh, yt: List[A])) =>
        val d = (xh, yh)
        go(xt, yt, Cons(d, acc))
    }

    go(this, ys, Nil)
  }

  // (7)
  def partition[A](xs: List[A])(pred: A => Boolean): (List[A],List[A]) = {
    def togo(xs: List[A])(left: List[A], right: List[A]): (List[A], List[A]) = xs match {
      case Nil => (left.reverse(left), right.reverse(right))
      case Cons(xh, xt) =>
      if pred(xh) then togo(xt)(Cons(xh, left), right)
      else
       togo(xt)(left, Cons(xh, right))
 } // (8)
    togo(xs)(Nil,Nil)
  }

  // (9)
  def reverse[A](xs: List[A]): List[A] = {
    // (10)
    def togo_1(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(xh, xt) => togo_1(xt, Cons(xh, acc))
    }
    togo_1(xs, Nil)
  }
  // (11)
  override def toString = foldRight("") { case (a, s) => s"$a $s" }

import List.*
object List:
  def empty[A]: List[A] = Nil
  def of[A](xs: A*): List[A] = xs.foldRight(Nil: List[A])(Cons(_, _))

// (!!!!!!!)
@main def run = {
  println("Hello")
}
