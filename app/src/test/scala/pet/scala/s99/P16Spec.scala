package pet.scala.s99

class P16Spec extends PxxSpec {

  private def drop[T](n: Int, elems: List[T]): List[T] =
    if (n <= 0) throw new IllegalArgumentException
    else if (n == 1) List.empty
    else 
      elems.zipWithIndex.filter { 
        case (elem, index) => (((index+1) % n) != 0)
      }.map {
        case (elem, _) => elem
      }

  behavior of "'drop' method"

  it should "not change empty lists" in {
    drop(1, List.empty) shouldBe List.empty
    drop(2, List.empty) shouldBe List.empty
    drop(3, List.empty) shouldBe List.empty
  }

  it should "drop every n-th element of the list" in {
    drop(2, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 3, 5)
    drop(3, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 4, 5)
    drop(4, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 3, 5, 6)
    drop(5, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 3, 4, 6)
    drop(6, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 3, 4, 5)
  }

  it should "drop every element if n is 1" in {
    drop(1, List(1)) shouldBe List.empty
    drop(1, List(1, 2)) shouldBe List.empty
    drop(1, List(1, 2, 3)) shouldBe List.empty
  }

  it should "not drop any elements if n is greater than list size" in {
    drop(2, List(1)) shouldBe List(1)
    drop(3, List(1, 2)) shouldBe List(1, 2)
    drop(4, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "throw an exception if n is less than 0" in {
    an[IllegalArgumentException] should be thrownBy drop(-1, List.empty)
    an[IllegalArgumentException] should be thrownBy drop(-1, List(1))
    an[IllegalArgumentException] should be thrownBy drop(-1, List(1, 2))
  }

  it should "throw an exception if n is 0" in {
    an[IllegalArgumentException] should be thrownBy drop(0, List.empty)
    an[IllegalArgumentException] should be thrownBy drop(0, List(1))
    an[IllegalArgumentException] should be thrownBy drop(0, List(1, 2))
  }

  it should "drop elements from a huge list" in {
    drop(2, List.fill(500000)(List(1, 2)).flatten) shouldBe List.fill(500000)(1)
  }
}