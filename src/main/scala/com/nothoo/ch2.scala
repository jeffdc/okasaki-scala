package com.nothoo

object Chapter2 {
    /** Fig 2.1 Signature for Stacks */
    trait Stack[+T] {
        def isEmpty: Boolean
        def cons[U >: T](x: U) : Stack[U]
        def head: T
        def tail: Stack[T]
    }

    /** Allow the List itself to be a function. */
    object ListBackedStack {
      def apply[T] : ListBackedStack[T] = new ListBackedStack[T](List())
    }

    /** Figure 2.2 Implementation of stacks using teh built-in types. */
    class ListBackedStack[T] (private val l: List[T]) extends Stack[T] {
        def isEmpty = l.isEmpty
        def cons[U >: T](x: U) = new ListBackedStack(x::l)
        def head: T = l.head
        def tail = new ListBackedStack(l.tail)
    }

    /** Figure 2.3 Implementation od stacks using a custom datatype. */
    sealed abstract class CustomStack[+T] extends Stack[T]
    case object EmptyCustomStack extends CustomStack[Nothing] {
        def isEmpty = true
        def cons[T](x: T) = new NotEmptyCustomStack[T](x, EmptyCustomStack)
        def head = throw new IllegalArgumentException("No head of an empty Stack.")
        def tail = throw new IllegalArgumentException("No tail of an empty Stack.")
    }
    final case class NotEmptyCustomStack[+T](head: T, tail: CustomStack[T]) extends CustomStack[T] {
        def isEmpty = false
        def cons[U >: T](x: U) = new NotEmptyCustomStack(x, this)
    }

    /** catenate operator: ++ */
    def ++[T](xs: List[T], ys: List[T]) : List[T] = xs match {
        case Nil => ys
        case x::xs => x :: (++(xs,ys))
    }

    /** update function */
    def update[T](xs: List[T], i: Int, y: T): List[T] = (xs, i) match {
        case (Nil, _) => throw new AssertionError("Not possible to update an empty List.")
        case (x::xs, 0) => y :: xs
        case (x::xs, i) => x :: update(xs, i - 1, y)
    }

    /** Exercise 2.1: Suffixes */
    def suffixes[T](xs: List[T]): List[List[T]] = xs match {
        case Nil => Nil :: Nil
        case x::xs =>  (x :: xs) :: suffixes(xs)
    }


    trait BinarySearchTree[+T] {

    }
}