package pet.scala.s99

class P15Spec extends PxxSpec {

  private def replicate[T](replicationFactor: Int, elems: List[T]): List[T] = 
    if (replicationFactor < 0) throw new IllegalArgumentException
    else
      elems.flatMap {
        case elem => List.fill(replicationFactor)(elem)
      }

  behavior of "'replicate' method"

  it should "not change empty lists" in {
    replicate(0, List.empty) shouldBe List.empty
    replicate(1, List.empty) shouldBe List.empty
    replicate(2, List.empty) shouldBe List.empty
    replicate(3, List.empty) shouldBe List.empty
  }

  it should "throw an exception if replication factor is less than 0" in {
    an[IllegalArgumentException] should be thrownBy replicate(-1, List(1))
    an[IllegalArgumentException] should be thrownBy replicate(-1, List(1, 2))
    an[IllegalArgumentException] should be thrownBy replicate(-1, List(1, 1))
  }

  it should "return empty list if replication factor is 0" in {
    replicate(0, List(1)) shouldBe List.empty
    replicate(0, List(1, 2)) shouldBe List.empty
    replicate(0, List(1, 1)) shouldBe List.empty
  }

  it should "not change lists if replication factor is 1" in {
    replicate(1, List(1)) shouldBe List(1)
    replicate(1, List(1, 2)) shouldBe List(1, 2)
    replicate(1, List(1, 1)) shouldBe List(1, 1)
  }

  it should "replicate each element of a list n-times" in {
    replicate(2, List(1)) shouldBe List(1, 1)
    replicate(2, List(1, 2)) shouldBe List(1, 1, 2, 2)
    replicate(2, List(1, 1)) shouldBe List(1, 1, 1, 1)
    replicate(3, List(1)) shouldBe List(1, 1, 1)
    replicate(3, List(1, 2)) shouldBe List(1, 1, 1, 2, 2, 2)
    replicate(3, List(1, 1)) shouldBe List(1, 1, 1, 1, 1, 1)
  }

  it should "replicate each element of a huge list" in {
    replicate(1, List.fill(1000000)(1)) shouldBe List.fill(1000000)(1)
    replicate(2, List.fill(1000000)(1)) shouldBe List.fill(2000000)(1)
    replicate(3, List.fill(1000000)(1)) shouldBe List.fill(3000000)(1)
  }

  it should "replicate each element of a list with huge replication factor" in {
    replicate(1000000, List(1)) shouldBe List.fill(1000000)(1)
  }

  it should "replicate each element of a big list even if " +
    "replication factor is also big" in {
    replicate(1000, List.fill(1000)(1)) shouldBe List.fill(1000000)(1)
  }
}