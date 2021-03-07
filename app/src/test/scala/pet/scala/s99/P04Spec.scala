package pet.scala.s99

class P04Spec extends PxxSpec {

  private def length[T](elems: List[T]): Int =
    elems.foldLeft(0) { (accumulator, _) => accumulator + 1 }

  behavior of "'length' method"

  it should "get the length of a list" in {
    length(List.empty) shouldBe 0
    length(List(1)) shouldBe 1
    length(List(1, 2)) shouldBe 2
    length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

  it should "get the length of a huge list" in {
    length(List.fill(1000000)(0)) shouldBe 1000000
  }
}