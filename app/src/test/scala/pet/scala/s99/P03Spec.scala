package pet.scala.s99

import scala.annotation.tailrec

class P03Spec extends PxxSpec {
 
  private def nth[T](n: Int, elems: Seq[T]): T = {    
    @tailrec
    def nthRecursive[T](n: Int, elems: Seq[T]): T =
      (n, elems) match {
        case (0, head :: _) => head
        case (index, _ :: tail) => nthRecursive(index-1, tail)
      }
    
    if (n < 0) throw new IllegalArgumentException
    if (n >= elems.size) throw new NoSuchElementException
    nthRecursive(n, elems)
  }

  behavior of "'nth' method"

  it should "find the n-th element of a list" in {
      nth(0, List(0, 1, 2)) shouldBe 0
      nth(1, List(0, 1, 2)) shouldBe 1
      nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
  }
  
  it should "throw exception for an empty list" in {
    an[NoSuchElementException] should be thrownBy nth(0, List.empty)
    an[NoSuchElementException] should be thrownBy nth(1, List.empty)
    an[NoSuchElementException] should be thrownBy nth(2, List.empty)
  }
  
  it should "throw exception for a negative index" in {
    an[IllegalArgumentException] should be thrownBy nth(-1, List.empty)
    an[IllegalArgumentException] should be thrownBy nth(-1, List(1, 2))
  }
  
  it should "throw exception for an index equal or higher than list size" in {
    an[NoSuchElementException] should be thrownBy nth(1, List(0))
    an[NoSuchElementException] should be thrownBy nth(2, List(0, 1))
    an[NoSuchElementException] should be thrownBy nth(3, List(0, 1))
  }

  it should "find the n-th element of a huge list" in {
    nth(1000000, List.fill(1000000)(0) :+ 1) shouldBe 1
  }

}