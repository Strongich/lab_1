package com.tkroman.kpi.y2022.l1

import scala.collection.mutable
import munit.FunSuite

import List.*

class ListSuite extends FunSuite {
  test("foldRight") {
    val expected = -94
    val xs = Cons(1,Cons(3,Cons(8,Nil)))
    val actual = xs.foldRight(100)(_-_)
    assertEquals(expected, actual)
  }

  test("concat") {
    val expected = Cons("a",Cons("b",Cons("c",Cons("d",Cons("f",Nil)))))
    val ys = Cons("d",Cons("f",Nil))
    val xs = Cons("a",Cons("b",Cons("c",Nil)))
    val actual = xs.concat(xs,ys)
    assertEquals(expected, actual)
  }

  test("flatmap") {
    val expected = Cons("1",Cons("1",Cons("1",Cons("2",Cons("2",Cons("2",Cons("3",Cons("3",Cons("3",Nil)))))))))
    val a = Cons(1, Cons(2, Cons(3,Nil)))
    val actual = a.flatMap(a)(x=> Cons(x.toString,Cons(x.toString,Cons(x.toString,Nil))))
    assertEquals(expected, actual)
  }

  test("zip") {
    val expected = Cons((1,'a'),Cons((2,'b'),Cons((3,'c'),Nil)))
    val xs = Cons(1,Cons(2,Cons(3,Nil)))
    val ys = Cons('a',Cons('b',Cons('c',Cons('d',Nil))))
    val actual = xs.zip(ys)
    assertEquals(expected, actual)
  }
  test("partition") {
    val expected = (Cons(3,Cons(6,Nil)),Cons(1,Cons(2,Cons(4,Cons(5,Cons(7,Nil))))))
    val xs = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Nil)))))))
    val actual = xs.partition(xs)(x => x%3 == 0)
    assertEquals(expected,actual)
  }
}




