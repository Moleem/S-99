package pet.scala.s99

import scala.annotation.tailrec

class P18Spec extends PxxSpec {

  private def slice[T](start: Int, end: Int, elems: List[T]): List[T] = {
    @tailrec
    def sliceRecursive[T](elems: List[T], ctr: Int=0, acc: List[T]=Nil): List[T] =
      if (elems.isEmpty || ctr >= end) acc.reverse
      else sliceRecursive(
        elems.tail, ctr + 1,
        if (ctr >= start && ctr < end) elems.head :: acc
        else acc
      )

    if (start < 0 || end < 0 || start > end) 
      throw new IllegalArgumentException
    else
      sliceRecursive(elems)
  }
  
  behavior of "'slice' method"

  it should "not change an empty list" in {
    slice(0, 0, List.empty) shouldBe List.empty
    slice(0, 5, List.empty) shouldBe List.empty
    slice(2, 5, List.empty) shouldBe List.empty
  }

  it should "extract a slice from a list not including last index" in {
    slice(0, 2, List(0, 1, 2)) shouldBe List(0, 1)
    slice(1, 3, List(0, 1, 2)) shouldBe List(1, 2)
    slice(0, 3, List(0, 1, 2)) shouldBe List(0, 1, 2)
  }

  it should "allow overindexing" in {
    slice(0, 5, List(0, 1, 2)) shouldBe List(0, 1, 2)
    slice(1, 5, List(0, 1, 2)) shouldBe List(1, 2)
    slice(2, 5, List(0, 1, 2)) shouldBe List(2)
    slice(3, 5, List(0, 1, 2)) shouldBe List.empty
  }

  it should "return empty list if two bounds are the same" in {
    slice(0, 0, List(0, 1, 2)) shouldBe List.empty
    slice(1, 1, List(0, 1, 2)) shouldBe List.empty
    slice(2, 2, List(0, 1, 2)) shouldBe List.empty
    slice(3, 3, List(0, 1, 2)) shouldBe List.empty
  }

  it should "throw an exception if either bound is negative" in {
    an[IllegalArgumentException] should be thrownBy slice (-1, 0, List(1, 2))
    an[IllegalArgumentException] should be thrownBy slice (0, -1, List(1, 2))
    an[IllegalArgumentException] should be thrownBy slice (-1, -1, List(1, 2))
  }

  it should "throw an exception if lower bound is higher than upper bound" in {
    an[IllegalArgumentException] should be thrownBy slice (1, 0, List(1, 2))
    an[IllegalArgumentException] should be thrownBy slice (2, 1, List(1, 2))
    an[IllegalArgumentException] should be thrownBy slice (3, 0, List(1, 2))
  }

  it should "slice huge lists" in {
    slice(0, 3, List.fill(1000000)(1)) shouldBe List(1, 1, 1)
    slice(999997, 1000000, List.fill(1000000)(1)) shouldBe List(1, 1, 1)
  }
}