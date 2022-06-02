package com.tkroman.kpi.y2022.l1
import scala.collection.mutable
import munit.FunSuite

import List.*

class ListSuite extends FunSuite {
  test("foldRight") {
    val expected = -94
    val actual = List.of(1,3,8).foldRight(100)(_-_)
    assertEquals(expected,actual)
  }

  test("foldRight new realization for 100k") {
    val expected = 50000
    val actual = List.of().range(100000).foldRight2(List.of().range(100000),0)(_-_)
    assertEquals(expected,actual)
  }

  test("String Builder for Nil"){
    val expected = Nil
    val actual = List.of()
    assertEquals(expected,actual)
  }


  test("reverse"){
    val expected = List.of(3,2,1)
    val actual = List.of(1,2,3).reverse
    assertEquals(expected,actual)
  }

  test("concat") {
    val expected = List.of("a","b","c","d","f")
    val ys = List.of("d","f")
    val xs = List.of("a","b","c")
    val actual = xs.concat(xs,ys)
    assertEquals(expected, actual)
  }

  test("flatmap") {
    val expected = List.of("1","1","1","2","2","2","3","3","3")
    val a = List.of(1,2,3)
    val actual = a.flatMap(x=> List.of(x.toString,x.toString,x.toString))
    assertEquals(expected, actual)
  }

  test("zip") {
    val expected = List.of((1,'a'),(2,'b'),(3,'c'))
    val xs = List.of(1,2,3)
    val ys = List.of('a','b','c','d')
    val actual = xs.zip(ys)
    assertEquals(expected, actual)
  }
  test("partition") {
    val expected = (List.of(3,6),List.of(1,2,4,5,7))
    val xs = List.of(1,2,3,4,5,6,7)
    val actual = xs.partition(x => x%3 == 0)
    assertEquals(expected,actual)
  }
}




