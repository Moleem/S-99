package pet.scala.s99.lists

import pet.scala.s99.PxxSpec

class P14Spec extends PxxSpec {

  private def duplicate[T](elems: List[T]): List[T] = 
    elems.flatMap {
      case elem => List.fill(2)(elem)
    }

  behavior of "'duplicate' method"

  it should "not change empty lists" in {
    duplicate(List.empty) shouldBe List.empty
  }

  it should "duplicate each element of a list" in {
    duplicate(List(1)) shouldBe List(1, 1)
    duplicate(List(1, 2)) shouldBe List(1, 1, 2, 2)
    duplicate(List(1, 1)) shouldBe List(1, 1, 1, 1)
  }

  it should "duplicate each element of a huge list" in {
    duplicate(List.fill(1000000)(1)) shouldBe List.fill(2000000)(1)
  }
}