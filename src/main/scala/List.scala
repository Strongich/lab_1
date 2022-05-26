package com.tkroman.kpi.y2022.l1

import scala.collection.mutable

enum List[+A]:
  case Nil extends List[Nothing]
  case Cons(h: A, tl: List[A]) extends List[A]

  def foldRight[B](z: B)(f: (A, B) => B): B = { // rdy
    this match
      case Nil => z
      case Cons(h: A, tl: List[A]) => f(h, foldRight(z)(f))
  }

  def concat[A](xs: List[A], ys: List[A]): List[A] = {
    // tail recursive
    /*    xs match
      case Nil => ys
      case Cons(h: A, tl: List[A]) => concat(tl, ys) */
    // foldRight
    xs match
      case Nil => ys
      case Cons(xh,xt) => Cons(xs,Cons(ys,Nil)).flatMap(identity)
  }

  def flatMap[B](f: A => List[B]): List[B] = {
    // tail recursive
   /*  this match
      case Nil => Nil
      case Cons(h: A, tl: List[A]) => tl.flatMap(f) */

    // fold right
    def go[B](xs: List[A], acc: List[B], f: A => List[B]): List[B] = acc match {
      case Nil => acc
      case Cons(xh: A,xt: List[A]) => xs.foldRight(acc)((xh,acc)=>f(xh))
    }
    go(Nil,Nil,f)
  }

  def zip[B](ys: List[B]): List[(A,B)] =
    def go(xs: List[A], ys: List[B], acc: List[(A, B)]): List[(A, B)] = (xs, ys) match {
      case (Nil, _) | (_, Nil) => acc
      case (Cons(xh, xt), Cons(yh, yt)) =>
      val d = (xh, yh)
      go(xt, yt, Cons(d, acc))
 }
    go(Nil, ys, Nil)

  def partition[A](xs: List[A])(pred: A => Boolean): (List[A],List[A]) = {
    def togo(xs: List[A])(left: List[A], right: List[A]): (List[A], List[A]) = xs match {
      case Nil => (left.reverse(left), right.reverse(right))
      case Cons(xh, xt) =>
      if pred(xh) then togo(xt)(Cons(xh, left), right)
      else
       togo(xt)(left, Cons(xh, right))
 }
    togo(xs)(Nil,Nil)
}
  def reverse[A](xs: List[A]): List[A] =
    def togo_1(xs:List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case Cons(xh,xt) => togo_1(Cons(xh,acc),xt)
  }
    togo_1(xs,Nil)


// NOTE: do not use this to demo the results.
// Use unit-tests instead
@main def run() = {
println("Hello")





}