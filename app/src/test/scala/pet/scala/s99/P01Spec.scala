package pet.scala.s99

import scala.annotation.tailrec

class P01Spec extends PxxSpec {

  @tailrec
  private def last[T](elems: List[T]): T =
    elems match {
      case onlyElement :: Nil => onlyElement
      case _ :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }

  behavior of "'last' method"

  it should "find the last element of a non-empty list" in {
    last(List(1)) shouldBe 1
    last(List(1, 2)) shouldBe 2
    last(List(1, 2, 3)) shouldBe 3
    last(List(1, 2, 3, 4, 5, 8)) shouldBe 8
  }

  it should "throw exception for an empty list" in {
    an[NoSuchElementException] should be thrownBy last(List.empty)
  }

  it should "find the last element of a huge list" in {
    last(List.fill(1000000)(0) :+ 1) shouldBe 1
  }

}
