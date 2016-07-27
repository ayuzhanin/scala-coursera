package quickcheck

import common._

import org.scalacheck.{Properties, Arbitrary, Gen, Prop}
import Prop.forAll
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val emptyHeap = Gen.const(empty)

  lazy val genHeap: Gen[H] = for {
    value <- Arbitrary.arbitrary[A]
    heap <- Gen.oneOf(emptyHeap, genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get
    * the smallest of the two elements back
    */
  property("hint1") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == min(a, b)
  }

  /**
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap should be empty.
    */
  property("hint2") = forAll { (elem: Int) =>
    val single = insert(elem, empty)
    isEmpty(deleteMin(single))
  }

  /**
    * Given any heap, you should get a sorted sequence of elements
    * when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("hint3") = forAll { (heap: H) =>
    def isSorted(remained: H, prevMin: A): Boolean = {
      if (isEmpty(remained)) true
      else {
        val curMin = findMin(remained)
        prevMin < curMin && isSorted(deleteMin(remained), curMin)
      }
    }

    isSorted(heap, findMin(heap))
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other
    */
  property("hint4") = forAll { (a: H, b: H) =>
    findMin(meld(a , b)) == min(findMin(a), findMin(b))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
