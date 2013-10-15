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

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  // trait TestSets {
  //   val bound = 1000
  //   def arbitrarySet(i: Int*): Set = x => i.contains(x)
  //   def boundedSet(m: Int, n: Int): Set =  x => x >= m && x <= n
  //   val s1 = singletonSet(1)
  //   val s2 = singletonSet(2)
  //   val s3 = singletonSet(3)
  //   val s12 = arbitrarySet(1,2)
  //   val s13 = arbitrarySet(1,3)
  //   val s123 = arbitrarySet(1,2,3)
  //   def emptySet: Set = x => false
  //   val s1to10 = boundedSet(1,10)
  //   val underBounds = singletonSet(-(bound+1))
  //   val overBounds = singletonSet(bound+1)
  //   def id = (x: Int) => true
  // }

  // test("singletonSet(1) contains 1") {
  //   new TestSets {
  //     assert(contains(s1, 1), "Singleton")
  //     assert(!contains(s1, 2), "Singleton")
  //   }
  // }
}