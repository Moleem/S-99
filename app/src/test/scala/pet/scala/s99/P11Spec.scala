package pet.scala.s99

class P11Spec extends PxxSpec {

  private def encodeModified[T](elems: List[T]): List[Any] = 
    elems.foldRight(List[(Int, T)]()) { 
      case (currentElem, (elemCounter, elem) :: tail) if currentElem == elem =>
        (elemCounter + 1, elem) :: tail
      case (currentElem, accumulator) =>
        (1, currentElem) :: accumulator
    } map {
      case (1, elem) => elem
      case aggregate => aggregate
    }

  behavior of "'encodeModified' method"

  it should "not change empty lists" in {
    encodeModified(List.empty) shouldBe List.empty
  }

  it should "not change lists without duplicates" in {
    encodeModified(List(1)) shouldBe List(1)
    encodeModified(List(1, 2)) shouldBe List(1, 2)
    encodeModified(List(1, 2, 3)) shouldBe List(1, 2, 3)
  }
  
  it should "encode consecutive repeated elements into number-element tuples " +
    "but leave single elements alone" in {
    encodeModified(List(1, 1)) shouldBe List((2, 1))
    encodeModified(List(1, 2, 2, 3)) shouldBe List(1, (2, 2), 3)
    encodeModified(List(1, 1, 2, 2)) shouldBe List((2, 1), (2, 2))
    encodeModified(List(1, 1, 2, 1)) shouldBe List((2, 1), 2, 1)
  }

  it should "not confuse single and repeated elemenst" in {
    encodeModified(List((2, 1), 1, 1)) shouldBe List((2, 1), (2, 1))
    encodeModified(List(1, 1, (2, 1))) shouldBe List((2, 1), (2, 1))
  }

  it should "encode huge lists" in {
    encodeModified(List.fill(1000000)(1)) shouldBe List((1000000, 1))
    encodeModified(List.fill(500000)(List(1, 1, 2)).flatten) shouldBe 
      List.fill(500000)(List((2, 1), 2)).flatten
  }
}