package pet.scala.s99.lists

import pet.scala.s99.PxxSpec

class P25Spec extends PxxSpec {
  import P23Spec.randomSelect
  
  private def randomPermute[T](elems: List[T]): List[T] = 
    randomSelect(elems.size, elems)

  behavior of "'randomPermute' method"

  it should "generate a random permutation of the elemnts of a list" in {
    randomPermute(List(1)) should contain theSameElementsAs List(1)
    randomPermute(List(1, 2)) should contain theSameElementsAs List(1, 2)
    randomPermute(List(1, 2, 3)) should contain theSameElementsAs List(1, 2, 3)
  }

  it should "not change an empty list" in {
    randomPermute(List.empty) shouldBe List.empty
  }

  it should "shuffle a big list" in {
    randomPermute(List.fill(999)(1)) shouldBe List.fill(999)(1)
  }
 }