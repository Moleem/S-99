package pet.scala.s99

class P08Spec extends PxxSpec {

  private def compress[T](elems: List[T]): List[T] = 
    elems.foldRight(List[T]()) { (elem, accumulator) => 
      if (accumulator.headOption.contains(elem)) accumulator
      else elem :: accumulator
    }

  behavior of "'compress' method"

  it should "not change lists without duplicates" in {
    compress(List.empty) shouldBe List.empty
    compress(List(1)) shouldBe List(1)
    compress(List(1, 2)) shouldBe List(1, 2)
    compress(List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "remove consecutive duplication of elements within lists" in {
    compress(List(1, 1)) shouldBe List(1)
    compress(List(1, 2, 2, 3)) shouldBe List(1, 2, 3)
    compress(List(1, 1, 2, 2)) shouldBe List(1, 2)
    compress(List(1, 1, 2, 1)) shouldBe List(1, 2, 1)
  }

  it should "deduplicate huge lists" in {
    compress(List.fill(1000000)(1)) shouldBe List(1)
    compress(List.fill(500000)(List(1, 1, 2)).flatten) shouldBe List.fill(500000)(List(1, 2)).flatten
  }
}