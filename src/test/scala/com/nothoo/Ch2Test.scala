package com.nothoo

import org.scalatest.FunSuite

import org.scalatest.junit.JUnitRunner

class Ch2Suite extends FunSuite {

  import Chapter2._

  test("catenation works") {
    assert(++(Nil, Nil) == Nil)
    assert(++(Nil, List(1,2,3)) == List(1,2,3))
    assert(++(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
    assert(++(List(1,2,3), List(1,4,5)) == List(1,2,3,1,4,5))
  }

  test("update works") {
    assert(update(List(1), 0, 0) == List(0))
    assert(update(List(1,2,3), 2, 6) == List(1,2,6))
  }

  test("suffixes works") {
    assert(suffixes(List(1,2,3,4)) == List(List(1,2,3,4), List(2,3,4), List(3,4), List(4), Nil))
  }

  test("see how sweet the Unbalanced set is") {
    val s = new UnbalancedSet[Int].insert(1).insert(9).insert(7).insert(3)
    assert(s.member(1))
    assert(s.member(9))
    assert(s.member(7))
    assert(s.member(3))
    assert(!s.member(40))
    assert(!s.member(2))
  }

  test("size does matter") {
    assert(new UnbalancedSet[Int].insert(1).insert(2).size == 2)
  }

  test("complete function is totally working") {
    // size should 2^n - 1
    assert(complete(1, 10).size == scala.math.pow(2, 10) - 1)
  }

  test("balanced is not like politics, it works") {
    assert(balanced(1, 10).size == 10)
    assert(balanced(1, 9).size == 9)
  }
}