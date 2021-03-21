package pet.scala.s99.lists

import pet.scala.s99.PxxSpec

import scala.annotation.tailrec

class P17Spec extends PxxSpec {

  private def split[T](n: Int, elems: List[T]): (List[T], List[T]) = {
    @tailrec
    def splitRecursive[T](
      elems: List[T], counter: Int, acc: List[T] = Nil): (List[T], List[T]) =
      (elems, counter) match {
        case (Nil, _) => 
          (acc.reverse, Nil)
        case (elems, 0) => 
          (acc.reverse, elems)
        case (headElem :: tail, counter) =>
          splitRecursive(tail, counter - 1, headElem :: acc) 
      }

    if (n < 0) throw new IllegalArgumentException
    else splitRecursive(elems, n)
  }


  behavior of "'split' method"

  it should "split a list into two parts given the length of the first part" in {
    split(1, List(1, 2, 3)) shouldBe (List(1), List(2, 3))
    split(2, List(1, 2, 3)) shouldBe (List(1, 2), List(3))
    split(3, List(1, 2, 3)) shouldBe (List(1, 2, 3), List.empty)
  }

  it should "throw an exception if given length is less than 0" in {
    an[IllegalArgumentException] should be thrownBy split(-1, List.empty)
    an[IllegalArgumentException] should be thrownBy split(-1, List(1))
    an[IllegalArgumentException] should be thrownBy split(-1, List(1, 2))
  }

  it should "should return the original list (2nd pos) if given length is 0" in {
    split(0, List.empty) shouldBe (List.empty, List.empty)
    split(0, List(1)) shouldBe (List.empty, List(1))
    split(0, List(1, 2)) shouldBe (List.empty, List(1, 2))
  }

  it should "should return the original list (1st pos) if given length is " +
    "greater than list size (best effort approach)" in {
    split(1, List.empty) shouldBe (List.empty, List.empty)
    split(2, List(1)) shouldBe (List(1), List.empty)
    split(3, List(1, 2)) shouldBe (List(1, 2), List.empty)
  }

  it should "split huge lists" in {
    split(600000, List.fill(1000000)(1)) shouldBe
      (List.fill(600000)(1), List.fill(400000)(1))
  }
}