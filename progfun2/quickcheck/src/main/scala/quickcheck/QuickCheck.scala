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

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get
    * the smallest of the two elements back
    */
  property("hint1") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    if (a > b) findMin(heap) == b
    else findMin(heap) == a
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
    def isSorted(remained: H): Boolean = {
      if (isEmpty(remained)) true
      else {
        val min = findMin(remained)
        val reduced = deleteMin(remained)
        isSorted(reduced) || min < findMin(reduced)
      }
    }

    isSorted(heap)
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other
    */
  property("hint4") = forAll { (a: H, b: H) =>
    val minimum = min(findMin(a), findMin(b))
    minimum == findMin(meld(a , b))
  }

}
