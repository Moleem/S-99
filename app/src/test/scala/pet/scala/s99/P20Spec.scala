package pet.scala.s99

import scala.annotation.tailrec

object P20Spec {
  def removeAt[T](n: Int, elems: List[T]): (List[T], T) = {
    @tailrec
    def removeAtRecursive[T](elems: List[T], ctr: Int=0, acc: List[T]=Nil): (List[T], T) =
      elems match {
        case Nil => throw new NoSuchElementException
        case head :: tail =>
          if (ctr == n) (acc.reverse ::: tail, head)
          else removeAtRecursive(tail, ctr+1, head :: acc)
      }
    
    if (n < 0) throw new IllegalArgumentException
    removeAtRecursive(elems)
  }
}

class P20Spec extends PxxSpec {
  import P20Spec.removeAt

  behavior of "'removeAt' method"

  it should "remove the K-th element from a list and return it in a tuple" in {
    removeAt(0, List(0, 1, 2)) shouldBe (List(1, 2), 0)
    removeAt(1, List(0, 1, 2)) shouldBe (List(0, 2), 1)
    removeAt(2, List(0, 1, 2)) shouldBe (List(0, 1), 2)
  }

  it should "throw an exception if index is negative" in {
    an[IllegalArgumentException] should be thrownBy removeAt(-1, List.empty)
    an[IllegalArgumentException] should be thrownBy removeAt(-1, List(1, 2, 3))
    an[IllegalArgumentException] should be thrownBy removeAt(-2, List(1, 2, 3))
  }

  it should "throw an exception if index doesnt exist" in {
    a[NoSuchElementException] should be thrownBy removeAt(0, List.empty)
    a[NoSuchElementException] should be thrownBy removeAt(1, List(0))
    a[NoSuchElementException] should be thrownBy removeAt(2, List(0, 1))
  }

  it should "remove the K-th element from a huge list" in {
    removeAt(0, 0 :: List.fill(1000000)(1)) shouldBe 
      (List.fill(1000000)(1), 0)
    removeAt(1000000, List.fill(1000000)(1) ::: List(0)) shouldBe 
      (List.fill(1000000)(1), 0)
  }
}