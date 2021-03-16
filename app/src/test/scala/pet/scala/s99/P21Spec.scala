package pet.scala.s99

import scala.annotation.tailrec

class P21Spec extends PxxSpec {
  private def insertAt[T](newElem: T, position: Int, elems: List[T]): List[T] = {
    @tailrec
    def insertAtRecursive(pos: Int, es: List[T], acc: List[T] = Nil): List[T] =
      (es, pos) match {
        case (restOfElems, 0) => acc.reverse ::: newElem :: restOfElems
        case (_, p) if p < 0 => throw new IllegalArgumentException
        case (Nil, _) => throw new IllegalArgumentException
        case (head :: tail, p) => insertAtRecursive(p-1, tail, head :: acc)
      }

    insertAtRecursive(position, elems)
  }

  behavior of "'insertAt' method"

  it should "insert an element at a given position into a list" in {
    insertAt(0, 0, List(1, 2, 3, 4)) shouldBe List(0, 1, 2, 3, 4)
    insertAt(0, 1, List(1, 2, 3, 4)) shouldBe List(1, 0, 2, 3, 4)
    insertAt(0, 2, List(1, 2, 3, 4)) shouldBe List(1, 2, 0, 3, 4)
  }

  it should "throw an exception in case of underindexing" in {
    an[IllegalArgumentException] should be thrownBy insertAt(-1, 1, List.empty)
    an[IllegalArgumentException] should be thrownBy insertAt(-1, 2, List(1))
    an[IllegalArgumentException] should be thrownBy insertAt(-1, 3, List(1, 2))
  }

  it should "throw an exception in case of overindexing" in {
    an[IllegalArgumentException] should be thrownBy insertAt(0, 1, List.empty)
    an[IllegalArgumentException] should be thrownBy insertAt(0, 2, List(1))
    an[IllegalArgumentException] should be thrownBy insertAt(0, 3, List(1, 2))
  }

  it should "insert elements into a huge list" in {
    insertAt(0, 1000000, List.fill(1000000)(1)) shouldBe
      List.fill(1000000)(1) :+ 0
  }
}