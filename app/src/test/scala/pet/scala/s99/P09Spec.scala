package pet.scala.s99

class P09Spec extends PxxSpec {

  private def pack[T](elems: List[T]): List[List[T]] = 
    elems.foldRight(List[List[T]]()) { (elem, accumulator) =>
      if (accumulator.headOption.exists(_.contains(elem)))
        (elem :: accumulator.head) :: accumulator.tail
      else List(elem) :: accumulator
    }

  behavior of "'pack' method"

  it should "not change empty lists" in {
    pack(List.empty) shouldBe List.empty
  }

  it should "nest lists without duplicates" in {
    pack(List(1)) shouldBe List(List(1))
    pack(List(1, 2)) shouldBe List(List(1), List(2))
    pack(List(1, 2, 3)) shouldBe List(List(1), List(2), List(3))
  }

  it should "pack consecutive duplication of elements within lists" in {
    pack(List(1, 1)) shouldBe List(List(1, 1))
    pack(List(1, 2, 2, 3)) shouldBe List(List(1), List(2, 2), List(3))
    pack(List(1, 1, 2, 2)) shouldBe List(List(1, 1), List(2, 2))
    pack(List(1, 1, 2, 1)) shouldBe List(List(1, 1), List(2), List(1))
  }

  it should "pack huge lists" in {
    pack(List.fill(1000000)(1)) shouldBe List(List.fill(1000000)(1))
    pack(List.fill(500000)(List(1, 1, 2)).flatten) shouldBe List.fill(500000)(List(List(1, 1), List(2))).flatten
  }
}