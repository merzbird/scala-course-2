package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      v <- arbitrary[A]
      h <- oneOf(const(insert(v, empty)), genHeap)
    } yield meld(h, insert(k, h))
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a1: Int, a2: Int) =>
    val min = findMin(insert(a2, insert(a1, empty)))
    min == a1 || min == a2
  }

  property("min3") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    isEmpty(melded) || !isEmpty(h1) && findMin(melded) == findMin(h1) || !isEmpty(h2) && findMin(melded) == findMin(h2)
  }

  property("delete") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sorted") = forAll { h: H =>
    def checkMin(h: H, m: A): Boolean = isEmpty(h) || findMin(h) >= m && checkMin(deleteMin(h), findMin(h))

    isEmpty(h) || checkMin(deleteMin(h), findMin(h))
  }

  property("min4") = forAll { a: Int =>
    val b = a / 2
    deleteMin(insert(b + 1, insert(b, insert(b + 2, empty)))) == insert(b + 2, insert(b + 1, empty))
  }

}
