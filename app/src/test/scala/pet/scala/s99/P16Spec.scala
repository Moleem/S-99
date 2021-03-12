package pet.scala.s99

import scala.annotation.tailrec

class P16Spec extends PxxSpec {

  private def drop[T](n: Int, elems: List[T]): List[T] = {
    @tailrec
    def dropRecursive[T](elems: List[T], i: Int=1, acc: List[T]=Nil): List[T] =
      (elems, i) match {
        case (Nil, _) => acc
        case (head :: tail, i)  => 
          if (i % n == 0)
            dropRecursive(tail, i+1, acc)
          else 
            dropRecursive(tail, i+1, head :: acc)
      }

    if (n < 0) throw new IllegalArgumentException
    else if (elems.isEmpty) elems
    else if (n == 0) elems.tail
    else if (n == 1) List.empty
    else dropRecursive(elems).reverse
  }

  behavior of "'drop' method"

  it should "not change empty lists" in {
    drop(0, List.empty) shouldBe List.empty
    drop(1, List.empty) shouldBe List.empty
    drop(2, List.empty) shouldBe List.empty
  }

  it should "drop every n-th element of the list" in {
    drop(2, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 3, 5)
    drop(3, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 4, 5)
    drop(4, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 3, 5, 6)
    drop(5, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 3, 4, 6)
    drop(6, List(1, 2, 3, 4, 5, 6)) shouldBe List(1, 2, 3, 4, 5)
  }

  it should "drop only the first element if n is 0" in {
    drop(0, List(1)) shouldBe List.empty
    drop(0, List(1, 2)) shouldBe List(2)
    drop(0, List(1, 2, 3)) shouldBe List(2, 3)
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

  it should "drop elements from a huge list" in {
    drop(2, List.fill(500000)(List(1, 2)).flatten) shouldBe List.fill(500000)(1)
  }
}