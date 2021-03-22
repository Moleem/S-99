package pet.scala.s99.logic

import pet.scala.s99.PxxSpec

class P47Spec extends PxxSpec {
  import S99Logic._ 
  
  "'and' operator" should "implement logical AND" in {
    false and false shouldBe false
    false and true  shouldBe false
    true  and false shouldBe false
    true  and true  shouldBe true
  }
  
  "'or' operator" should "implement logical OR" in {
    false or false shouldBe false
    false or true  shouldBe true
    true  or false shouldBe true
    true  or true  shouldBe true
  }
  
  "'nand' operator" should "implement logical NAND" in {
    false nand false shouldBe true
    false nand true  shouldBe true
    true  nand false shouldBe true
    true  nand true  shouldBe false
  }
  
  // "'NOR' operator" should "implement logical NOR" in {
  //   Nfalse or false shouldBe true
  //   Nfalse or true  shouldBe false
  //   Ntrue or  false shouldBe false
  //   Ntrue or  true  shouldBe false
  // }
  
  // "'XOR' operator" should "implement logical XOR" in {
  //   Xfalse or false shouldBe false
  //   Xfalse or true  shouldBe true
  //   Xtrue or  false shouldBe true
  //   Xtrue or  true  shouldBe false
  // }
  
  // "'IMPL' operator" should "implement logical IMPL" in {
  //   IMPL(false, false) shouldBe true
  //   IMPL(false, true)  shouldBe true
  //   IMPL(true,  false) shouldBe false
  //   IMPL(true,  true)  shouldBe true
  // }
  
  // "'EQU' operator" should "implement logical EQU" in {
  //   EQU(false, false) shouldBe true
  //   EQU(false, true)  shouldBe false
  //   EQU(true,  false) shouldBe false
  //   EQU(true,  true)  shouldBe true
  // }
}