package pet.scala.s99

/*
 k ; elemsize ; result
 0 ; 0        ; empty
 0 ; 1        ; empty
 0 ; n        ; empty
 =============
 1 ; 0        ; not possible
 2 ; 0        ; not possible
 2 ; 1        ; not possible
 3 ; 0        ; not possible
 3 ; 1        ; not possible
 3 ; 2        ; not possible
 k ; <k       ; not possible
 =============
 1 ; 1        ; List(List(onlyelem))
 2 ; 2        ; List(List(onlyPair))
 3 ; 3        ; List(List(onlyTriplet))
 =============
 1 ; n        ; List(List(1stElem), List(2ndElem) ... List(nthElem))
 =============
 2 ; n        ; List(
                  List(1stElem+c(1, restAfter1st)), 
                  List(2ndElem+c(1, restAfter2nd)), 
                  ...,
                  List(n-2thElem+c(1, restAfterN-2nd)), 
                  List(n-1thElem, nthElem))
 3 ; n        ; List(
                  List(1stElem+c(2, restAfter1st)),
                  List(2ndElem+c(2, restAfter2nd)),
                  List(3rdElem+c(2, restAfter3rd)),
                  ...,
                  List(n-3rdElem+c(2, restAfterN-3rd)),
                  List(n-2ndElem, n-1stElem, nthElem)
                )
 
*/



class P26Spec extends PxxSpec { 
  private def combinations[T](k: Int, elems: List[T]): List[List[T]] = {
    val elemsSize = elems.size
    if (k == 0) List.empty
    else if (k == elemsSize) List(elems)
    else if (k == 1) elems.map(elem => List(elem))
    else if (k > elemsSize) List.empty
    else {
      combinations(k-1, elems.tail).map(c => elems.head :: c) ::: combinations(k, elems.tail)
    }
  }

  behavior of "'combinations' method"

  it should "return no results, if combination size is 0" in {
    combinations(0, List.empty) shouldBe List.empty
    combinations(0, List(1)) shouldBe List.empty
    combinations(0, List(1, 2)) shouldBe List.empty
  }

  it should "return no results, if combination size is greater than list size" in {
    combinations(1, List.empty) shouldBe List.empty
    combinations(2, List(1)) shouldBe List.empty
    combinations(3, List(1, 2)) shouldBe List.empty
  }

  it should "return original list wrapped, if combination size equals list size" in {
    combinations(1, List(1)) shouldBe List(List(1))
    combinations(2, List(1, 2)) shouldBe List(List(1, 2))
    combinations(3, List(1, 2, 3)) shouldBe List(List(1, 2, 3))
  }

  it should "return each element wrapped, if combination size is 1" in {
    combinations(1, List(1)) shouldBe List(List(1))
    combinations(1, List(1, 2)) shouldBe List(List(1), List(2))
    combinations(1, List(1, 2, 3)) shouldBe List(List(1), List(2), List(3))
  }

  it should "return possible combinations, if combination size is 2" in {
    val results = combinations(2, List(1, 2, 3))
    results should contain (List(1, 2))
    results should contain (List(1, 3))
    results should contain (List(2, 3))
    results.size shouldBe 3
  }

  it should "return possible combinations, if combination size is 3" in {
    val results = combinations(3, List(1, 2, 3, 4))
    results should contain (List(1, 2, 3))
    results should contain (List(1, 2, 4))
    results should contain (List(1, 3, 4))
    results should contain (List(2, 3, 4))
    results.size shouldBe 4
  }
}