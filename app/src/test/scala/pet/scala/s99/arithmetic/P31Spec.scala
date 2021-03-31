package pet.scala.s99.arithmetic

import pet.scala.s99.PxxSpec

object P31Spec {
  val primes = 2 #:: LazyList.from(3, 2).filter(_.isPrime)

  implicit class IntWithPrime(num: Int) {
    def isPrime: Boolean =
      num > 1 &&
      primes.takeWhile(_ <= Math.sqrt(num)).forall(num % _ != 0)
  }
}

class P31Spec extends PxxSpec {
  import P31Spec.IntWithPrime

  behavior of "'isPrime' method"

  it should "appear as a method of Int-s" in {
    2.isPrime shouldBe true
  }

  it should "identify prime numbers" in {
    2.isPrime shouldBe true
    3.isPrime shouldBe true
    5.isPrime shouldBe true
    7.isPrime shouldBe true
    11.isPrime shouldBe true
    13.isPrime shouldBe true
  }

  it should "identify big prime numbers" in {
    // 7919 is the 1000th prime number
    7919.isPrime shouldBe true
  
  }

  it should "identify non-prime numbers" in {
    0.isPrime shouldBe false
    1.isPrime shouldBe false
    4.isPrime shouldBe false
    6.isPrime shouldBe false
  }

  it should "identify big non-prime numbers" in {
    10000.isPrime shouldBe false
  }

  it should "identify negative non-prime numbers" in {
    -1.isPrime shouldBe false
    -2.isPrime shouldBe false
    -3.isPrime shouldBe false
  }
}