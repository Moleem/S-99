package pet.scala.s99

import scala.annotation.tailrec

class P04Spec extends PxxSpec {

  private def length[T](elems: Seq[T]): Int = {
    @tailrec
    def lengthRecursive[T](elems: Seq[T], accumulator: Int = 0): Int =
      elems match {
        case Nil => accumulator
        case head :: tail => lengthRecursive(tail, accumulator+1)
      }

    lengthRecursive(elems)
  }

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