package pet.scala.s99

class P13Spec extends PxxSpec {

  private def encodeDirect[T](elems: List[T]): List[(Int, T)] =
    elems.foldRight(List[(Int, T)]()) { 
      case (currentElem, (elemCounter, elem) :: tail) if currentElem == elem =>
        (elemCounter + 1, elem) :: tail
      case (currentElem, accumulator) =>
        (1, currentElem) :: accumulator
    }

  behavior of "'encodeDirect' method"

  it should "not change empty lists" in {
    encodeDirect(List.empty) shouldBe List.empty
  }

  it should "encode lists without duplicates" in {
    encodeDirect(List(1)) shouldBe List((1, 1))
    encodeDirect(List(1, 2)) shouldBe List((1, 1), (1, 2))
    encodeDirect(List(1, 2, 3)) shouldBe List((1, 1), (1, 2), (1, 3))
  }
  
  it should "encode consecutive elements into number-element tuples" in {
    encodeDirect(List(1, 1)) shouldBe List((2, 1))
    encodeDirect(List(1, 2, 2, 3)) shouldBe List((1, 1), (2, 2), (1, 3))
    encodeDirect(List(1, 1, 2, 2)) shouldBe List((2, 1), (2, 2))
    encodeDirect(List(1, 1, 2, 1)) shouldBe List((2, 1), (1, 2), (1, 1))
  }

  it should "encode huge lists" in {
    encodeDirect(List.fill(1000000)(1)) shouldBe List((1000000, 1))
    encodeDirect(List.fill(500000)(List(1, 1, 2)).flatten) shouldBe 
      List.fill(500000)(List((2, 1), (1, 2))).flatten
  }
}