package pet.scala.s99.arithmetic

import pet.scala.s99.PxxSpec

class P34Spec extends PxxSpec {
  import P33Spec.IntWithCoPrime

  implicit class IntWithTotient(num: Int) {
    def totient: Int = (1 to num).count(num.isCoprimeTo)
  }

  behavior of "'totient' method"

  it should "return the number of positive integers that are coprime to m" in {
    1.totient shouldBe 1
    2.totient shouldBe 1
    3.totient shouldBe 2
    4.totient shouldBe 2
    97.totient shouldBe 96
    98.totient shouldBe 42
    99.totient shouldBe 60
    100.totient shouldBe 40
  }

  it should "not yield results for non-positive m" in {
    0.totient shouldBe 0
    -1.totient shouldBe 0
    -2.totient shouldBe 0
    -3.totient shouldBe 0
  }

  it should "return the number of positive integers that are coprime to a huge m" in {
    1000000.totient shouldBe 400000
  }
}