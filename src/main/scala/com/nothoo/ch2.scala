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

    /** Figure 2.7 Signature for sets. */
    trait Set[T] {
        def insert(x: T): Set[T]
        def member(x: T): Boolean
        def size: Int
    }

    /** Figure 2.9 Implementation of Binary Search Tree. */
    sealed abstract class BTree[+T] {
        // makes testing easier - plus in the real world what we would we do with out this!?
        def size: Int
    }
    case object BTreeLeaf extends BTree[Nothing] {
        def size = 0
    }
    final case class BTreeNode[T](left: BTree[T], value: T, right: BTree[T]) extends BTree[T] {
        def size = left.size + 1 + right.size
    }

    // Exercise 2.2 - Speed UnbalancedSet.member up from 2d comparison to d + 1 comparisons where d = depth of tree
    // by keeping track of candidate elements that might be equal and not checking equality until hitting the bottom of the tree

    // Exercise 2.3 - Rewrite insert to throw an exception to avoid copying the search path unnecessarily
    class Exists extends RuntimeException
    // Exercise 2.4 Comnine ideas from 2.2 and 2.3 to rewite insert to not uncessarily copy and no more than d+1 comparisons

    // an UnbalancedSet class - use implicits to handle ordering
    class UnbalancedSet[T] private (t: BTree[T])(implicit val ordering: Ordering[T]) extends Set[T] {
        def this()(implicit ordering: Ordering[T]) = this(BTreeLeaf)(ordering)
        def newSet(t: BTree[T]) = new UnbalancedSet[T](t)
// rewritten for 2.3 and 2.4 - def insert(x: T): Set[T] = newSet(UnbalancedSet.insert(x, t))
        def insert(x: T): Set[T] = try {
            newSet(UnbalancedSet.insert(x, t, None))
        } catch {
            case _:Exists => this
        }
// rewritten for 2.2 - def member(x: T): Boolean = UnbalancedSet.member(x, t)
        def member(x: T): Boolean = UnbalancedSet.member(x, t, None)
        def size = t.size
    }

    // the functions that implement the set operations
    private object UnbalancedSet {
        private def member[T](x: T, t: BTree[T], c: Option[T])(implicit ordering: Ordering[T]): Boolean = (t,c) match {
            case (BTreeLeaf, None) => false
            case (BTreeLeaf, Some(d)) => ordering.equiv(x, d)
            case (BTreeNode(a, y, b),  _) =>
                if (ordering.lt(x, y)) member(x, a, c)
                else member(x, b, Some(y))
        }

        private def insert[T](x: T, t: BTree[T], c: Option[T])(implicit ordering: Ordering[T]): BTree[T] = (t,c) match {
            case (BTreeLeaf, None) => BTreeNode(BTreeLeaf, x, BTreeLeaf)
            case (BTreeLeaf, Some(d)) =>
                if (ordering.equiv(x, d)) throw new Exists
                else BTreeNode(BTreeLeaf, x, BTreeLeaf)
            case (BTreeNode(a, y, b), _) =>
                if (ordering.lt(x, y)) BTreeNode(insert(x, a, c), y, b)
                else BTreeNode(a, y, insert(x, b, Some(y)))
        }

// rewritten for 2.2
//        private def member[T](x: T, t: BTree[T])(implicit ordering: Ordering[T]): Boolean = t match {
//            case BTreeLeaf => false
//            case BTreeNode(a, y, b) =>
//                if (ordering.lt(x, y)) member(x, a)
//                else if (ordering.lt(y, x)) member(x, b)
//                else true

// rewritten for 2.3 and 2.4
//        private def insert[T](x: T, t: BTree[T])(implicit ordering: Ordering[T]): BTree[T] = t match {
//            case BTreeLeaf => BTreeNode(BTreeLeaf, x, BTreeLeaf)
//           case BTreeNode(a, y, b) =>
//                if (ordering.lt(x, y)) BTreeNode(insert(x, a), y, b)
//                else if (ordering.lt(y, x)) BTreeNode(a, y, insert(x, b))
//                else t
//        }
    }

    // Exercise 2.5 -
    // a) complete function
    def complete[T](x: T, size: Int): BTree[T] = size match {
        case 0 => BTreeLeaf
        case _ => {
            val sub = complete(x, size - 1)
            BTreeNode(sub, x, sub)
        }
    }

    // b) balanced function
    def balanced[T](x: T, size: Int)(implicit ordering: Ordering[T]): BTree[T] = {
        def recur(size: Int): (BTree[T],BTree[T]) = size match {
            case 0 => (BTreeLeaf, BTreeNode(BTreeLeaf, x, BTreeLeaf))
            // even
            case s if (size % 2 == 0) => recur(size / 2 - 1) match {
                case (l,r) => (BTreeNode(l, x, r), BTreeNode(r, x, r))
            }
            // odd
            case _ => recur(size / 2) match {
                case (l,r) => (BTreeNode(l, x, l), BTreeNode(l, x, r))
            }
        }
        recur(size)._1
    }

    // Exercise 2.6 - Port UnbalancedSet to support finite Maps rather than Sets.
    // seems tedious, maybe I am missing something.
}