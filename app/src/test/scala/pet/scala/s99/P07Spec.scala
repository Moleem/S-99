package pet.scala.s99

import scala.annotation.tailrec

class P07Spec extends PxxSpec {

  private def flatten(elems: List[Any]): List[Any] = {
    @tailrec
    def flattenRecursive(elems: List[Any], accumulator: List[Any] = Nil): List[Any] =
      elems match {
        case Nil => accumulator
        case (head: List[_]) :: Nil => flattenRecursive(head, accumulator)
        case (head: List[_]) :: tail => flattenRecursive(head ::: tail, accumulator)
        case head :: tail => flattenRecursive(tail, accumulator :+ head)
      }

    flattenRecursive(elems)
  }

  behavior of "'flatten' method"

  it should "flatten a list structure with only non-list elements in it" in {
    flatten(List(1, 1)) shouldBe List(1, 1)
  }

  it should "flatten a list structure with only list elements in it" in {
    flatten(List(List(1), List(1))) shouldBe List(1, 1)
    flatten(List(List(1, 2), List(3))) shouldBe List(1, 2, 3)
    flatten(List(List(1), List(2, 3))) shouldBe List(1, 2, 3)
  }

  it should "flatten a list structure with list and non-list elements mixed in it" in {
    flatten(List(List(1), 1)) shouldBe List(1, 1)
    flatten(List(1, List(1))) shouldBe List(1, 1)
    flatten(List(List(1, 2), 3)) shouldBe List(1, 2, 3)
    flatten(List(1, List(2, 3))) shouldBe List(1, 2, 3)
  }

  it should "flatten a nested list structure" in {
    flatten(List(List(List(List(1)), List(2)), List(3))) shouldBe List(1, 2, 3)
    flatten(List(List(List(1), 2), 3)) shouldBe List(1, 2, 3)
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  it should "flatten a list structure with empty list like elements in it" in {
    flatten(List.empty) shouldBe List.empty
    flatten(List(List.empty)) shouldBe List.empty
    flatten(List(List.empty, List.empty)) shouldBe List.empty
  }

  it should "flatten a huge list structure" in {
    flatten(List.fill(500)(1)) shouldBe List.fill(500)(1)
    flatten(List.fill(500)(List(1))) shouldBe List.fill(500)(1)
    flatten(List(List.fill(500)(1))) shouldBe List.fill(500)(1)
    val initialDeepList: List[Any] = List(1)
    val deepList: List[Any] = 
      (1 until 500).foldLeft(initialDeepList) { (li, _) => 
        List(li, List(1))
      }
    flatten(deepList) shouldBe List.fill(500)(1)
  }
}