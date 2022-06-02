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
  test("foldRight new realization for 10k") {
    val expected = Seq.range(1,10001).foldRight(0)(_-_)
    val actual = List.of().range(10000).foldRight2(0)(_-_)
    assertEquals(expected,actual)
  }
  test("foldRight new realization for 100k") {
    val expected = Seq.range(1,100001).foldRight(0)(_-_)
    val actual = List.of().range(100000).foldRight2(0)(_-_)
    assertEquals(expected,actual)
  }

  test("String Builder for Nil"){
    val expected = Nil
    val actual = List.of()
    assertEquals(expected,actual)
  }
  test("String Builder"){
    val expected = "[1, 2, 3, 4, 5, 6]"
    val actual = List.of(1,2,3,4,5,6).toString
    assertEquals(expected,actual)
  }


  test("reverse"){
    val expected = List.of(3,2,1)
    val actual = List.of(1,2,3).reverse
    assertEquals(expected,actual)
  }
  test("concat with same length") {
    val expected = List.of(1,2,3,4,5,6)
    val xs = List.of(1,2,3)
    val ys = List.of(4,5,6)
    val actual = xs.concat(xs,ys)
    assertEquals(expected, actual)
  }
  test("concat with diff length") {
    val expected = List.of("a","b","c","d","f")
    val ys = List.of("d","f")
    val xs = List.of("a","b","c")
    val actual = xs.concat(xs,ys)
    assertEquals(expected, actual)
  }

  test("flatmap for ints") {
    val expected = List.of("1","1","1","2","2","2","3","3","3")
    val a = List.of(1,2,3)
    val actual = a.flatMap(x=> List.of(x.toString,x.toString,x.toString))
    assertEquals(expected, actual)
  }
  test("flatmap for chars") {
    val expected = List.of("a","a","a","b","b","b","c","c","c")
    val a = List.of("a","b","c")
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




