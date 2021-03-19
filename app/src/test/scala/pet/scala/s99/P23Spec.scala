package pet.scala.s99

import P20Spec.removeAt
import scala.util.Random
import scala.annotation.tailrec

object P23Spec {
  private val random = Random
  def randomSelect[T](n: Int, elems: List[T]): List[T] = {
    @tailrec
    def randomSelectRecursive(n: Int, elemSize: Int, elems: List[T], acc: List[T]=Nil): List[T] = {
      (n, elems) match {
        case (0, _) => acc
        case (count, es) =>
          val index = random.nextInt(elemSize)
          val (rest, elem) = removeAt(index, es)
          randomSelectRecursive(n-1, elemSize-1, rest, elem :: acc)
      }
    }

    val elemSize = elems.size
    if (n < 0 || n > elemSize) throw new IllegalArgumentException
    else if (n == elemSize) elems
    else randomSelectRecursive(n, elemSize, elems)
  }
}

class P23Spec extends PxxSpec {
  import P23Spec.randomSelect  

  behavior of "'randomSelect' method"

  private def verifyRandomSelection[T](n: Int, elems: List[T]): Unit = {
    val randomElems = randomSelect(n, elems)
    randomElems.size shouldBe n
    randomElems.toSet.size shouldBe n
    randomElems.foreach(elem => elems should contain(elem))
  }

  it should "extract a given number of randomly selected elements from a list" in {
    verifyRandomSelection(1, List(1, 2, 3, 4))
    verifyRandomSelection(2, List(1, 2, 3, 4))
    verifyRandomSelection(3, List(1, 2, 3, 4))
    verifyRandomSelection(4, List(1, 2, 3, 4))
  }

  it should "return an empty list if result size is set to 0" in {
    randomSelect(0, List.empty) shouldBe List.empty
    randomSelect(0, List(1)) shouldBe List.empty
    randomSelect(0, List(1, 2)) shouldBe List.empty
    randomSelect(0, List(1, 2, 3)) shouldBe List.empty
  }

  it should "return an original list if result size equals list size" in {
    randomSelect(0, List.empty) shouldBe List.empty
    randomSelect(1, List(1)) shouldBe List(1)
    randomSelect(2, List(1, 2)) shouldBe List(1, 2)
    randomSelect(3, List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  it should "throw an exception if result size is negative" in {
    an[IllegalArgumentException] should be thrownBy randomSelect(-1, List.empty)
    an[IllegalArgumentException] should be thrownBy randomSelect(-1, List(1))
    an[IllegalArgumentException] should be thrownBy randomSelect(-1, List(1, 2))
    an[IllegalArgumentException] should be thrownBy randomSelect(-1, List(1, 2, 3))
  }

  it should "throw an exception if result size exceeds input size" in {
    an[IllegalArgumentException] should be thrownBy randomSelect(1, List.empty)
    an[IllegalArgumentException] should be thrownBy randomSelect(2, List(1))
    an[IllegalArgumentException] should be thrownBy randomSelect(3, List(1, 2))
    an[IllegalArgumentException] should be thrownBy randomSelect(4, List(1, 2, 3))
  }

  it should "extract random elements from a huge list" in {
    randomSelect(1, List.fill(1000000)(1))
    randomSelect(1000000, List.fill(1000000)(1)) shouldBe 
      List.fill(1000000)(1)
  }

  it should "extract a lot of random elements from a big list" in {
    randomSelect(999, List.fill(1000)(1)) shouldBe 
      List.fill(999)(1)
  }
}