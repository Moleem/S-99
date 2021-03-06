package pet.scala.s99

import scala.annotation.tailrec

class P06Spec extends PxxSpec {

  private def isPalindrome[T](elems: List[T]): Boolean = {
    @tailrec
    def isPalindromeRecursive[T](elems: List[T], accumulator: Boolean = true): Boolean =
      if (accumulator) elems match {
        case Nil | _ :: Nil => accumulator
        case first :: second :: Nil => isPalindromeRecursive(Nil, accumulator && (first == second))
        case first :: _ :: third :: Nil => isPalindromeRecursive(Nil, accumulator && (first == third))
        case first +: mid :+ last => isPalindromeRecursive(mid, accumulator && (first == last))
      }
      else accumulator

    isPalindromeRecursive(elems)   
  }

  behavior of "'isPalindrome' method"

  it should "tell if a list is palindrome" in {
    isPalindrome(List.empty) shouldBe true
    isPalindrome(List(1)) shouldBe true
    isPalindrome(List(1, 1)) shouldBe true
    isPalindrome(List(1, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  it should "tell if a list is not palindrome" in {
    isPalindrome(List(1, 2)) shouldBe false
    isPalindrome(List(1, 2, 3)) shouldBe false
    isPalindrome(List(1, 2, 3, 1, 2)) shouldBe false
  }

  // TODO: optimize for large input

  it should "tell if a huge list is not palindrome" in {
    isPalindrome(List.fill(1000000)(1) :+ 0) shouldBe false
  }
}