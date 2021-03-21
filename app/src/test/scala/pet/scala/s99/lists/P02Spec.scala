package pet.scala.s99.lists

import pet.scala.s99.PxxSpec

import scala.annotation.tailrec

class P02Spec extends PxxSpec {
  
  @tailrec
  private def penultimate[T](elems: List[T]): T =
    elems match {
      case lastButOne :: _ :: Nil => lastButOne 
      case _ :: tail => penultimate(tail)
      case _ => throw new NoSuchElementException
    }

  behavior of "'penultimate' method"

  it should "find the penultimate element of a non-empty list" in {
      penultimate(List(1, 2)) shouldBe 1
      penultimate(List(1, 2, 3)) shouldBe 2
      penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
  }

  it should "throw exception for lists with size less than 2" in {
      an[NoSuchElementException] should be thrownBy penultimate(List.empty)
      an[NoSuchElementException] should be thrownBy penultimate(List(1))
  }

  it should "find the penultimate element of a huge list" in {
    penultimate(List.fill(1000000)(0) :+ 1 :+ 0) shouldBe 1
  }

}