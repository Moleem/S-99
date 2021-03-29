package pet.scala.s99.arithmetic

import pet.scala.s99.PxxSpec

object P32Spec {
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)
}

class P32Spec extends PxxSpec {
  import P32Spec.gcd
  
  behavior of "'gcd' method"

  it should "determine the greatest common divisor of two positive integers" in {
    gcd(8, 12) shouldBe 4
    gcd(36, 63) shouldBe 9
    gcd(12, 8) shouldBe 4
    gcd(63, 36) shouldBe 9
  }

  it should "determine the greatest common divisor of two huge integers" in {
    gcd(999999,909090) shouldBe 90909
  }
}