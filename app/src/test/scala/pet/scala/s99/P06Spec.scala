package pet.scala.s99

class P06Spec extends PxxSpec {
  
  private def isPalindrome[T](elems: List[T]): Boolean = 
    elems == elems.reverse

  behavior of "'isPalindrome' method"

  it should "tell if a list is a palindrome" in {
    isPalindrome(List.empty) shouldBe true
    isPalindrome(List(1)) shouldBe true
    isPalindrome(List(1, 1)) shouldBe true
    isPalindrome(List(1, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  it should "tell if a list is not a palindrome" in {
    isPalindrome(List(1, 2)) shouldBe false
    isPalindrome(List(1, 2, 3)) shouldBe false
    isPalindrome(List(1, 2, 3, 1, 2)) shouldBe false
  }

  it should "tell if a huge list is a palindrome" in {
    isPalindrome(List.fill(1000000)(1)) shouldBe true
  }

  it should "tell if a huge list is not a palindrome" in {
    isPalindrome(List.fill(1000000)(1) :+ 0) shouldBe false
  }
}