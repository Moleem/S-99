package pet.scala.s99

import scala.annotation.tailrec

class P05Spec extends PxxSpec {

  private def reverse[T](elems: List[T]): List[T] = 
    elems.foldLeft(List[T]()) { (accumulator, elem) => elem :: accumulator }
  
  behavior of "'reverse' method"

  it should "reverse a list" in {
    reverse(List.empty) shouldBe List.empty
    reverse(List(1)) shouldBe List(1)
    reverse(List(1, 2)) shouldBe List(2, 1)
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  it should "reverse a huge list" in {
    reverse((1 to 1000000).toList) shouldBe (1000000 to 1 by -1).toList
  }
}