package pet.scala.s99.arithmetic

import pet.scala.s99.PxxSpec

object P33Spec {
  import P32Spec.gcd

  implicit class IntWithCoPrime(num: Int) {
    def isCoprimeTo(other: Int): Boolean =
      if (num < 0 || other < 0) throw new IllegalArgumentException
      else gcd(num, other) == 1
  }
}
class P33Spec extends PxxSpec {
  import P33Spec.IntWithCoPrime  

  behavior of "'coprime' method"

  it should "determine whether two positive integers are coprime" in {
    35 isCoprimeTo 64 shouldBe true
    3 isCoprimeTo 5 shouldBe true
    5 isCoprimeTo 8 shouldBe true
    8 isCoprimeTo 9 shouldBe true
  }

  it should "determine whether two positive integers are not coprime" in {
    2 isCoprimeTo 4 shouldBe false
    9 isCoprimeTo 12 shouldBe false
    8 isCoprimeTo 20 shouldBe false
  }

  it should "throw an exception on negative inputs" in {
    an[IllegalArgumentException] should be thrownBy (-2 isCoprimeTo 4)
    an[IllegalArgumentException] should be thrownBy (2 isCoprimeTo -4)
    an[IllegalArgumentException] should be thrownBy (-2 isCoprimeTo -4)
  }

  it should "determine whether two large integers are coprime" in {
    999999 isCoprimeTo 999998 shouldBe true
  }
}