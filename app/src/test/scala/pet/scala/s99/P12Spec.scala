package pet.scala.s99

class P12Spec extends PxxSpec {

  private def decode[T](elems: List[(Int, T)]): List[T] =
    elems.flatMap { 
      case (elemCounter, elem) => List.fill(elemCounter)(elem)
    }

  behavior of "'decode' method"

  it should "not change empty lists" in {
    decode(List.empty) shouldBe List.empty
  }

  it should "decode list of run-length encoded tuples" in {
    decode(List((1, 1))) shouldBe List(1)
    decode(List((2, 1))) shouldBe List(1, 1)
    decode(List((1, 1), (1, 2), (1, 3))) shouldBe List(1, 2, 3)
    decode(List((3, 1), (2, 2), (1, 3))) shouldBe List(1, 1, 1, 2, 2, 3)
  }

  it should "decode huge lists" in {
    decode(List.fill(500000)((1, 1))) shouldBe List.fill(500000)(1)
    decode(List.fill(500000)((2, 1))) shouldBe List.fill(1000000)(1)
    decode(List.fill(500000)(List((2, 1), (1, 2))).flatten) shouldBe 
      List.fill(500000)(List(1, 1, 2)).flatten
  }
}