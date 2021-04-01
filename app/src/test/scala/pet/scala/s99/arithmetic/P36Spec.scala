package pet.scala.s99.arithmetic

import pet.scala.s99.PxxSpec

class P36Spec extends PxxSpec {
  import P35Spec.IntWithPrimeFactors
  import pet.scala.s99.lists.P13Spec.encodeDirect

  implicit class IntWithPrimeFactorMultiplicity(num: Int) {
    def primeFactorMultiplicity: List[(Int, Int)] =
      encodeDirect(num.primeFactors).map{case (count, elem) => (elem, count)}
  }

  behavior of "'primeFactorMultiplicity' method"

  it should "determine the prime factors of 1" in {
    1.primeFactorMultiplicity shouldBe List((1, 1))
  }

  it should "determine the prime factors of a given positive integer" in {
    2.primeFactorMultiplicity shouldBe List((2, 1))
    3.primeFactorMultiplicity shouldBe List((3, 1))
    4.primeFactorMultiplicity shouldBe List((2, 2))
    5.primeFactorMultiplicity shouldBe List((5, 1))
    6.primeFactorMultiplicity shouldBe List((2, 1), (3, 1))
    315.primeFactorMultiplicity shouldBe List((3, 2), (5, 1), (7, 1))
  }

  it should "throw an exception on a non-positive input" in {
    an[IllegalArgumentException] should be thrownBy 0.primeFactors
    an[IllegalArgumentException] should be thrownBy -1.primeFactors
    an[IllegalArgumentException] should be thrownBy -2.primeFactors
  }

  it should "determine the prime factors of a huge number" in {
    2208558.primeFactorMultiplicity shouldBe List((2, 1), (3, 1), (11, 1), (109, 1), (307, 1))
  }
}