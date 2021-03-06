package pet.scala.s99.arithmetic

import scala.annotation.tailrec
import pet.scala.s99.PxxSpec

object P35Spec {
  import P31Spec.primes

  implicit class IntWithPrimeFactors(num: Int) {
    def primeFactors: List[Int] = {
      @tailrec
      def primeFactorsRecursive(
        n: Int, primes: LazyList[Int]=primes, factors: List[Int] = Nil
      ): List[Int] = {
        if (n == 1) factors
        else if (n % primes.head == 0)
          primeFactorsRecursive(n / primes.head, primes, primes.head :: factors)
        else 
          primeFactorsRecursive(n, primes.tail, factors)
      }

      if (num < 1) throw new IllegalArgumentException
      else if (num == 1) List(1)
      else primeFactorsRecursive(num).reverse
    }
  }
}
class P35Spec extends PxxSpec {
  import P35Spec.IntWithPrimeFactors

  behavior of "'primeFactors' mehod"

  it should "determine the prime factors of 1" in {
    1.primeFactors shouldBe List(1)
  }

  it should "determine the prime factors of a given positive integer" in {
    2.primeFactors shouldBe List(2)
    3.primeFactors shouldBe List(3)
    4.primeFactors shouldBe List(2, 2)
    5.primeFactors shouldBe List(5)
    6.primeFactors shouldBe List(2, 3)
    315.primeFactors shouldBe List(3, 3, 5, 7)
  }

  it should "throw an exception on a non-positive input" in {
    an[IllegalArgumentException] should be thrownBy 0.primeFactors
    an[IllegalArgumentException] should be thrownBy -1.primeFactors
    an[IllegalArgumentException] should be thrownBy -2.primeFactors
  }

  it should "determine the prime factors of a huge number" in {
    2208558.primeFactors shouldBe List(2, 3, 11, 109, 307)
  }
}