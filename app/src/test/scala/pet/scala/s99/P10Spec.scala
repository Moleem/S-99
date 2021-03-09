package pet.scala.s99

class P10Spec extends PxxSpec {

  private def encode[T](elems: List[T]): List[(Int, T)] =
    elems.foldRight(List[(Int, T)]()) { 
      case (currentElem, (elemCounter, elem) :: tail) if currentElem == elem =>
        (elemCounter + 1, elem) :: tail
      case (currentElem, accumulator) =>
        (1, currentElem) :: accumulator
    }

  behavior of "'encode' method"

  it should "not change empty lists" in {
    encode(List.empty) shouldBe List.empty
  }

  it should "encode lists without duplicates" in {
    encode(List(1)) shouldBe List((1, 1))
    encode(List(1, 2)) shouldBe List((1, 1), (1, 2))
    encode(List(1, 2, 3)) shouldBe List((1, 1), (1, 2), (1, 3))
  }
  
  it should "encode consecutive elements into number-element tuples" in {
    encode(List(1, 1)) shouldBe List((2, 1))
    encode(List(1, 2, 2, 3)) shouldBe List((1, 1), (2, 2), (1, 3))
    encode(List(1, 1, 2, 2)) shouldBe List((2, 1), (2, 2))
    encode(List(1, 1, 2, 1)) shouldBe List((2, 1), (1, 2), (1, 1))
  }

  it should "encode huge lists" in {
    encode(List.fill(1000000)(1)) shouldBe List((1000000, 1))
    encode(List.fill(500000)(List(1, 1, 2)).flatten) shouldBe 
      List.fill(500000)(List((2, 1), (1, 2))).flatten
  }
}