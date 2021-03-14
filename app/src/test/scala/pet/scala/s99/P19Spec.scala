package pet.scala.s99

class P19Spec extends PxxSpec {
  private def rotate[T](n: Int, elems: List[T]): List[T] = {
    val elemsSize = elems.size
    if (elemsSize == 0 || n == 0) elems
    else {
      val normalizedN = 
        if (n >= 0) n % elemsSize
        else elemsSize + (n % elemsSize)
      val (beforeN, afterN) = elems.splitAt(normalizedN)
      afterN ::: beforeN
    }
  }

  behavior of "'rotate' method"

  it should "not change empty lists" in {
    rotate(-2, List.empty) shouldBe List.empty
    rotate(0, List.empty) shouldBe List.empty
    rotate(2, List.empty) shouldBe List.empty
  }

  it should "rotate elements N places to the left" in {
    rotate(0, List(1, 2, 3)) shouldBe List(1, 2, 3)
    rotate(1, List(1, 2, 3)) shouldBe List(2, 3, 1)
    rotate(2, List(1, 2, 3)) shouldBe List(3, 1, 2)
    rotate(3, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "effectively rotate elements N places to the right if N is negative" in {
    rotate(-1, List(1, 2, 3)) shouldBe List(3, 1, 2)
    rotate(-2, List(1, 2, 3)) shouldBe List(2, 3, 1)
    rotate(-3, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "handle Ns larger than list size" in {
    rotate(4, List(1, 2, 3)) shouldBe List(2, 3, 1)
    rotate(5, List(1, 2, 3)) shouldBe List(3, 1, 2)
    rotate(6, List(1, 2, 3)) shouldBe List(1, 2, 3)
    rotate(-4, List(1, 2, 3)) shouldBe List(3, 1, 2)
    rotate(-5, List(1, 2, 3)) shouldBe List(2, 3, 1)
    rotate(-6, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "rotate huge lists" in {
    rotate(1000000, List.fill(1000000)(0) ::: List(1)) shouldBe 
      1 :: List.fill(1000000)(0)
    rotate(-1000000, 1 :: List.fill(1000000)(0)) shouldBe 
      List.fill(1000000)(0) ::: List(1)
  }
}