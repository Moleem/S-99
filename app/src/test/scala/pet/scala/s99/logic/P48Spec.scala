package pet.scala.s99.logic

import pet.scala.s99.PxxSpec

class P48Spec extends PxxSpec {
  private def gray(n: Int): List[String] = {
    if (n < 0) throw new IllegalArgumentException
    else if (n == 0) List.empty
    else if (n == 1) List("0", "1")
    else {
      val previous = gray(n-1)
      val newStartWith1 = previous.map(elem => "1" + elem)
      val newStartWith0 = previous.map(elem => "0" + elem)
      
      newStartWith0 ::: newStartWith1
    }
  }

  behavior of "'gray' method"

  it should "generate gray codes of size 1 in order" in {
    gray(1) shouldBe List("0", "1")
  }

  it should "generate gray codes of size 2 in order" in {
    gray(2) shouldBe List("00", "01", "10", "11")
  }

  it should "generate gray codes of size N in order" in {
    gray(3) shouldBe List("000", "001", "010", "011", "100", "101", "110", "111")
  }

  it should "return an empty list if requested size is 0" in {
    gray(0) shouldBe List.empty
  }

  it should "throw an exception if requested size is negative" in {
    an[IllegalArgumentException] should be thrownBy gray(-1)
    an[IllegalArgumentException] should be thrownBy gray(-2)
    an[IllegalArgumentException] should be thrownBy gray(-3)
  }

  // it should "generate huge lists" in {
  //   import scala.math.pow

  //   gray(10).size shouldBe pow(2, 10)
  // }
}