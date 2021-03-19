package pet.scala.s99

class P24Spec extends PxxSpec {
  import P23Spec.randomSelect  

  private def lotto(n: Int, m: Int): List[Int] = 
    randomSelect(n, (1 to m).toList)
  
  behavior of "'lotto' method"

  private def verifyRandomSelection[T](n: Int, m: Int): Unit = {
    val drawn = lotto(n, m)
    drawn.size shouldBe n
    drawn.toSet.size shouldBe n
    drawn.foreach { elem => 
      elem should be >= 1
      elem should be <= m
    }
  }

  it should "draw N random numbers from 1..M" in {
    verifyRandomSelection(1, 4)
    verifyRandomSelection(2, 4)
    verifyRandomSelection(3, 4)
    verifyRandomSelection(4, 4)
  }

  it should "return an empty list if result size is set to 0" in {
    lotto(0, 0) shouldBe List.empty
    lotto(0, 1) shouldBe List.empty
    lotto(0, 2) shouldBe List.empty
    lotto(0, 3) shouldBe List.empty
  }

  it should "throw an exception if result size is negative" in {
    an[IllegalArgumentException] should be thrownBy lotto(-1, 0)
    an[IllegalArgumentException] should be thrownBy lotto(-1, 1)
    an[IllegalArgumentException] should be thrownBy lotto(-1, 2)
    an[IllegalArgumentException] should be thrownBy lotto(-1, 3)
  }

  it should "throw an exception if result size exceeds input size" in {
    an[IllegalArgumentException] should be thrownBy lotto(1, 0)
    an[IllegalArgumentException] should be thrownBy lotto(2, 1)
    an[IllegalArgumentException] should be thrownBy lotto(3, 2)
    an[IllegalArgumentException] should be thrownBy lotto(4, 3)
  }

  it should "draw random numbers from a huge range" in {
    verifyRandomSelection(1, 1000000) 
  }

  it should "draw a lot of random numbers from a big range" in {
    verifyRandomSelection(999, 1000) 
  }
}