package pet.scala.s99.lists

import pet.scala.s99.PxxSpec

import scala.annotation.tailrec

class P22Spec extends PxxSpec {

  private def range(start: Int, end: Int): List[Int] = {
    @tailrec
    def rangeRecursive(position: Int, acc: List[Int] = Nil): List[Int] =
     if (position >= start) rangeRecursive(position - 1, position :: acc)
     else acc
    
    if (start > end) throw new IllegalArgumentException
    else rangeRecursive(end)

  }

  behavior of "'range' method"

  it should "generate a list contianing all integers within a given range" in {
    range(0, 0) shouldBe List(0)
    range(0, 1) shouldBe List(0, 1)
    range(0, 2) shouldBe List(0, 1, 2)
    range(1, 3) shouldBe List(1, 2, 3)
    range(-1, 0) shouldBe List(-1, 0)
    range(-2, 0) shouldBe List(-2, -1, 0)
    range(-3, -1) shouldBe List(-3, -2, -1)
  }

  it should "throw an exception if low bound is higher than high bound" in {
    an[IllegalArgumentException] should be thrownBy range(3, 1)
    an[IllegalArgumentException] should be thrownBy range(0, -2)
  }

  it should "generate huge lists" in {
    range(0, 1000000) shouldBe (0 to 1000000).toList
  }
}