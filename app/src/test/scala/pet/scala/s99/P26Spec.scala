package pet.scala.s99

class P26Spec extends PxxSpec { 
  private def combinations[T](k: Int, elems: List[T]): List[List[T]] = {
    val elemsSize = elems.size
    if (k == 0) List.empty
    else if (k == elemsSize) List(elems)
    else if (k == 1) elems.map(elem => List(elem))
    else if (k > elemsSize) List.empty
    else {
      combinations(k-1, elems.tail).map(c => elems.head :: c) ::: combinations(k, elems.tail)
    }
  }

  behavior of "'combinations' method"

  it should "return no results, if combination size is 0" in {
    combinations(0, List.empty) shouldBe List.empty
    combinations(0, List(1)) shouldBe List.empty
    combinations(0, List(1, 2)) shouldBe List.empty
  }

  it should "return no results, if combination size is greater than list size" in {
    combinations(1, List.empty) shouldBe List.empty
    combinations(2, List(1)) shouldBe List.empty
    combinations(3, List(1, 2)) shouldBe List.empty
  }

  it should "return original list wrapped, if combination size equals list size" in {
    combinations(1, List(1)) shouldBe List(List(1))
    combinations(2, List(1, 2)) shouldBe List(List(1, 2))
    combinations(3, List(1, 2, 3)) shouldBe List(List(1, 2, 3))
  }

  it should "return each element wrapped, if combination size is 1" in {
    combinations(1, List(1)) shouldBe List(List(1))
    combinations(1, List(1, 2)) shouldBe List(List(1), List(2))
    combinations(1, List(1, 2, 3)) shouldBe List(List(1), List(2), List(3))
  }

  it should "return possible combinations, if combination size is 2" in {
    val results = combinations(2, List(1, 2, 3))
    results should contain (List(1, 2))
    results should contain (List(1, 3))
    results should contain (List(2, 3))
    results.size shouldBe 3
  }

  it should "return possible combinations, if combination size is 3" in {
    val results = combinations(3, List(1, 2, 3, 4))
    results should contain (List(1, 2, 3))
    results should contain (List(1, 2, 4))
    results should contain (List(1, 3, 4))
    results should contain (List(2, 3, 4))
    results.size shouldBe 4
  }
}