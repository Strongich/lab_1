package com.tkroman.kpi.y2022.l1

import scala.collection.mutable
import munit.FunSuite

class ListSuite extends FunSuite {
  test("foldRight") {
    val expected = -94
    val actual = mutable.Buffer(1, 3, 8).foldRight(100)(_ - _)
    assertEquals(expected, actual)
  }
  test("concat") {
    val expected = mutable.ArrayBuffer('a', 'b', 'c', 'd', 'f')
    var ys = mutable.ArrayBuffer('d', 'f')
    var xs = mutable.ArrayBuffer('a', 'b', 'c')
    val actual = xs.concat(ys)
    assertEquals(expected, actual)
  }
  test("flatmap") {
    val expected = Vector('A', 'B', 'C', 'D', 'E', 'F', 'G', 'T', 'H')
    val a = Vector("abcde", "fgth")
    val actual = a.flatMap(_.toUpperCase)
    assertEquals(expected, actual)
  }
  test("zip") {
    val expected = mutable.Buffer((1,'a'), (2,'b'), (3,'c'))
    val xs = mutable.Buffer(1,2,3)
    val ys = mutable.Buffer('a','b','c','d')
    val actual = xs.zip(ys)
    assertEquals(expected, actual)
  }
  test("partition") {
    val expected = (mutable.Buffer(3, 6),mutable.Buffer(1, 2, 4, 5, 7))
    val xs = mutable.Buffer(1,2,3,4,5,6,7)
    val actual = xs.partition(x => x%3 == 0)
    assertEquals(expected,actual)
  }
}




