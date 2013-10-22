package com.nothoo

object Chapter3 {

    import Chapter2._

    class Empty extends RuntimeException

    // Figure 3.1 Signature for Heap (Priority Queue)
    trait Heap[+T] {
        def isEmpty: Boolean
        def insert(x: T): Heap
        def merge(other: Heap): Heap
        def findMin: T throws Empty
        def deleteMin: Heap throws Empty
    }

//WIP
    // TODO - need to decorate the BTree with rank information

    sealed abstract class LeftistHeap[+T] extends Heap[T]
    case object EmptyLeftistHeap extends LeftistHeap[Nothing] {
        def isEmpty = true
        def insert(x: T): Heap = new NotEmptyLeftistHeap()
        def merge(other: Heap): Heap = other
        def findMin: T throws Empty = throw new Empty
        def deleteMin: Heap throws Empty = throw new Empty
    }
    final case class NotEmptyLeftistHeap[+T](t: BTree) extends LeftistHeap[T] {
        def isEmpty = false
        def insert(x: T): Heap = LeftistHeap.merge(t, NotEmptyLeftistHeap(BTreeNode(BTreeLeaf, x, BTreeLeaf)))
        def merge(other: Heap): Heap = LeftistHeap.merge(t, other)
        def findMin: T throws Empty = t match {
            case BTreeLeaf => throw new Empty
            case (_, v, _) => v
        }
        def deleteMin: Heap throws Empty = t match {
            case BTreeLeaf => throw new Empty
            case (l, v, r) => LeftistHeap.merge(l, r)
        }
    }
    private object LeftistHeap {
        def insert[T](h: LeftistHeap[T], x: T)
        def merge[T](h1: LeftistHeap[T], h2: LeftistHeap[T])
    }

}